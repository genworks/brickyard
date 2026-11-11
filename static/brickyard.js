// Brickyard's page: an X_ITE canvas, a parts bin and a report.
//
// The server owns the build.  Each part arrives as its own small X3D
// document (a Transform pN holding a PlaneSensor pNs and the drawn part
// inside a Transform pNd), which is added to the scene's "parts" group.
// Dragging a part moves pNd with the sensor; on release the move goes to
// the server, which drops and snaps the part and sends it back, and the
// old nodes are swapped for the new.  The camera is never touched.

(function () {
  'use strict';

  const LDU = 0.4;                       // millimeters per LDraw unit
  const buildId = document.body.dataset.build;
  const canvas = document.getElementById('canvas');
  const messageBox = document.getElementById('message');
  const parts = new Map();               // id -> { outer, sensor, inner, label }
  let browser, group, selected = null, color = 'red', busy = Promise.resolve();
  let carried = [];                      // ids that move with the selected part, itself excluded
  let shift = false;                     // Shift held: the selected part moves alone
  let lastStatus = null;                 // the build's status as the server last sent it

  document.addEventListener('keydown', function (e) { if (e.key === 'Shift') { shift = true; preview(lastDrag); } });
  document.addEventListener('keyup', function (e) { if (e.key === 'Shift') { shift = false; preview(lastDrag); } });
  let lastDrag = null;                   // { id, x, y } while a drag is under way

  // Ask which parts come along with id; the answer only lights and
  // previews, the server decides again on release.
  function askCarried(id) {
    carried = [];
    fetch('/brickyard/api', { method: 'POST', body: new URLSearchParams({ b: buildId, op: 'carried', id: id }) })
      .then(function (r) { return r.json(); })
      .then(function (reply) {
        if (selected !== id || !reply.ok) return;
        carried = reply.carried.filter(function (c) { return c !== id; });
        highlight();
        preview(lastDrag);
      })
      .catch(function () {});
  }

  // Slide the dragged part, and what it carries unless Shift is held.
  function preview(drag) {
    parts.forEach(function (part, id) {
      const moving = drag && (id === drag.id || (!shift && carried.indexOf(id) >= 0));
      part.inner.translation = moving ? new X3D.SFVec3f(drag.x, drag.y, 0) : new X3D.SFVec3f(0, 0, 0);
    });
  }

  // One request at a time, in order: a drag's release and the key after
  // it must reach the server in the order they were made.
  function api(op, args) {
    const body = new URLSearchParams(Object.assign({ b: buildId, op: op }, args || {}));
    busy = busy.then(function () {
      return fetch('/brickyard/api', { method: 'POST', body: body })
        .then(function (r) { return r.json(); })
        .then(apply)
        .catch(function (e) { say('The server did not answer: ' + e); });
    });
    return busy;
  }

  function say(text) {
    messageBox.textContent = text || '';
    messageBox.classList.toggle('shown', !!text);
  }

  // Run: the server says what turns, about what line and how fast, and
  // the page turns it; nothing goes back and forth while it runs.  Each
  // part turns inside its own drawn Transform, about a center in the
  // model's own coordinates, so the part itself is not moved.
  const TURN = Math.PI;                  // a motor's radians a second: a turn in two seconds
  const runButton = document.getElementById('run');
  let running = null;                    // list of { part, axis, speed } while it runs

  function stopMotion() {
    if (!running) return;
    running.forEach(function (m) {
      m.part.inner.rotation = new X3D.SFRotation(0, 0, 1, 0);
      m.part.inner.center = new X3D.SFVec3f(0, 0, 0);
    });
    running = null;
    runButton.textContent = 'Run';
  }

  function turnFrame(now) {
    if (!running) return;
    if (turnFrame.start === null) turnFrame.start = now;
    const seconds = (now - turnFrame.start) / 1000;
    running.forEach(function (m) {
      const angle = (m.speed * TURN * seconds) % (2 * Math.PI);
      m.part.inner.rotation = new X3D.SFRotation(m.axis[0], m.axis[1], m.axis[2], angle);
    });
    requestAnimationFrame(turnFrame);
  }

  function startMotion(list) {
    running = list.filter(function (m) { return parts.has(m.id); }).map(function (m) {
      const part = parts.get(m.id);
      part.inner.center = new X3D.SFVec3f(m.point[0] * LDU, m.point[1] * LDU, m.point[2] * LDU);
      return { id: m.id, part: part, axis: m.axis, speed: m.speed };
    });
    runButton.textContent = 'Stop';
    turnFrame.start = null;
    requestAnimationFrame(turnFrame);
  }

  runButton.addEventListener('click', function () {
    if (running) { stopMotion(); say(''); return; }
    const body = new URLSearchParams({ b: buildId, op: 'motion' });
    if (selected !== null) body.set('id', selected);
    fetch('/brickyard/api', { method: 'POST', body: body })
      .then(function (r) { return r.json(); })
      .then(function (reply) {
        say(reply.message || '');
        if (reply.ok) startMotion(reply.motion);
      })
      .catch(function (e) { say('The server did not answer: ' + e); });
  });

  // The table: the build stands on it as a robot and runs a program.  The
  // server runs it and answers frames, whole numbers as they say in
  // table.lisp, and the page plays them back: the robot's Transform, each
  // turning part inside its own, and each piece.
  const tableToggle = document.getElementById('table-toggle');
  const tablePanel = document.getElementById('table-panel');
  const programText = document.getElementById('program');
  const programLog = document.getElementById('program-log');
  let table = null;                      // { node, pieces, center, lift, drives } while it is out

  function tableCall(args) {
    const body = new URLSearchParams(Object.assign({ b: buildId }, args));
    return fetch('/brickyard/api/table', { method: 'POST', body: body }).then(function (r) { return r.json(); });
  }

  function placeFrame(f) {
    const center = table.center, turning = table.drives;
    group.center = new X3D.SFVec3f(center[0], center[1], 0);
    group.translation = new X3D.SFVec3f(f[1] / 10 - center[0], f[2] / 10 - center[1], table.lift);
    group.rotation = new X3D.SFRotation(0, 0, 1, f[3] / 1000);
    turning.forEach(function (drive, i) {
      const angle = f[4 + i] / 1000;
      drive.forEach(function (p) {
        if (!parts.has(p.id)) return;
        const inner = parts.get(p.id).inner;
        inner.center = new X3D.SFVec3f(p.point[0] * LDU, p.point[1] * LDU, p.point[2] * LDU);
        inner.rotation = new X3D.SFRotation(p.axis[0], p.axis[1], p.axis[2], (p.speed * angle) % (2 * Math.PI));
      });
    });
    table.pieces.forEach(function (piece, i) {
      const at = 4 + turning.length + 3 * i;
      piece.translation = new X3D.SFVec3f(f[at] / 10, f[at + 1] / 10, 0);
      piece.rotation = new X3D.SFRotation(0, 0, 1, f[at + 2] / 1000);
    });
  }

  function playFrames(frames) {
    const run = table.run = {};
    let start = null, at = 0;
    function frame(now) {
      if (!table || table.run !== run) return;
      if (start === null) start = now;
      const ms = now - start;
      while (at + 1 < frames.length && frames[at + 1][0] <= ms) at++;
      placeFrame(frames[at]);
      if (at + 1 < frames.length) requestAnimationFrame(frame);
    }
    requestAnimationFrame(frame);
  }

  function logRun(reply) {
    programLog.textContent = (reply.steps || []).concat(reply.summary || []).join('\n');
  }

  async function openTable() {
    const reply = await tableCall({ action: 'open' });
    if (!reply.ok) { say(reply.message); return; }
    stopMotion();
    const scene = await browser.createX3DFromString(reply.x3d);
    const world = browser.currentScene.getNamedNode('world');
    const node = scene.getNamedNode('table');
    const pieces = [];
    for (let i = 0; i < reply.frames[0].length; i++) {
      let piece = null;
      try { piece = scene.getNamedNode('t' + i); } catch (e) { break; }
      pieces.push(piece);
    }
    world.addChildren = new X3D.MFNode(node);
    table = { node: node, pieces: pieces, center: reply.center, lift: reply.lift, drives: reply.drives };
    parts.forEach(function (part) { part.sensor.enabled = false; });
    placeFrame(reply.frames[0]);
    browser.currentScene.getNamedNode('table-eye').set_bind = true;
    tablePanel.hidden = false;
    runButton.hidden = true;
    tableToggle.textContent = 'Back to the build';
    say('');
  }

  function closeTable() {
    if (!table) return;
    browser.currentScene.getNamedNode('world').removeChildren = new X3D.MFNode(table.node);
    table.drives.forEach(function (drive) {
      drive.forEach(function (p) {
        if (!parts.has(p.id)) return;
        parts.get(p.id).inner.rotation = new X3D.SFRotation(0, 0, 1, 0);
        parts.get(p.id).inner.center = new X3D.SFVec3f(0, 0, 0);
      });
    });
    table = null;
    group.center = new X3D.SFVec3f(0, 0, 0);
    group.translation = new X3D.SFVec3f(0, 0, 0);
    group.rotation = new X3D.SFRotation(0, 0, 1, 0);
    parts.forEach(function (part) { part.sensor.enabled = true; });
    browser.currentScene.getNamedNode('eye').set_bind = true;
    tablePanel.hidden = true;
    runButton.hidden = false;
    tableToggle.textContent = 'Table';
    programLog.textContent = '';
  }

  tableToggle.addEventListener('click', function () {
    if (table) closeTable();
    else openTable().catch(function (e) { say('The table did not open: ' + e); });
  });

  // The same program as a Pybricks program for a real hub: shown, and
  // offered as a file.
  document.getElementById('program-python').addEventListener('click', function () {
    tableCall({ action: 'python', program: programText.value })
      .then(function (reply) {
        if (!reply.ok) { say(reply.message); return; }
        programLog.textContent = '';
        const link = document.createElement('a');
        link.href = URL.createObjectURL(new Blob([reply.python], { type: 'text/x-python' }));
        link.download = 'brickyard-' + buildId + '.py';
        link.textContent = 'Save as ' + link.download;
        const code = document.createElement('pre');
        code.textContent = reply.python;
        programLog.append(link, code);
      })
      .catch(function (err) { say('The server did not answer: ' + err); });
  });

  tablePanel.addEventListener('submit', function (e) {
    e.preventDefault();
    tableCall({ action: 'run', program: programText.value })
      .then(function (reply) {
        if (!table) return;
        say(reply.message || '');
        if (reply.ok) { logRun(reply); playFrames(reply.frames); }
      })
      .catch(function (err) { say('The server did not answer: ' + err); });
  });

  async function apply(reply) {
    say(reply.message || '');
    // a part that changed is drawn anew, and the plan no longer holds
    if ((reply.parts || []).length || (reply.removed || []).length) { stopMotion(); closeTable(); }
    removeParts(reply.removed || []);
    const changed = reply.parts || [];
    if (changed.length > 1) await putParts(changed);
    else for (const p of changed) await putPart(p);
    if (reply.status) report(reply.status);
    if (selected !== null && !parts.has(selected)) selected = null;
    highlight();
    if (selected !== null && ((reply.parts || []).length || (reply.removed || []).length)) askCarried(selected);
  }

  // In one assignment: of several in one frame only the last takes effect,
  // and the others' nodes stay on the screen with nothing behind them.
  function removeParts(ids) {
    const known = ids.filter(function (id) { return parts.has(id); });
    if (!known.length) return;
    group.removeChildren = new X3D.MFNode(...known.map(function (id) { return parts.get(id).outer; }));
    known.forEach(function (id) { parts.delete(id); });
  }

  async function putPart(p) {
    const scene = await browser.createX3DFromString(p.x3d);
    removeParts([p.id]);
    group.addChildren = new X3D.MFNode(wire(scene, p));
  }

  // Many parts at once (a whole build, a group moved together): their
  // documents' scenes are joined into one, parsed once, and added in one
  // step; parsing them one by one took half a minute for a few hundred.
  async function putParts(ps) {
    const open = '<Scene>', close = '</Scene>';
    const first = ps[0].x3d;
    const head = first.slice(0, first.indexOf(open) + open.length);
    const body = ps.map(function (p) {
      return p.x3d.slice(p.x3d.indexOf(open) + open.length, p.x3d.lastIndexOf(close));
    }).join('');
    const scene = await browser.createX3DFromString(head + body + close + '</X3D>');
    removeParts(ps.map(function (p) { return p.id; }));
    group.addChildren = new X3D.MFNode(...ps.map(function (p) { return wire(scene, p); }));
  }

  // Find part p's nodes in scene, record them and hook up its sensor;
  // returns the outer Transform to be added to the parts group.
  function wire(scene, p) {
    const outer = scene.getNamedNode('p' + p.id);
    const sensor = scene.getNamedNode('p' + p.id + 's');
    const inner = scene.getNamedNode('p' + p.id + 'd');
    const part = { outer: outer, sensor: sensor, inner: inner, label: p.label, dragged: null };
    parts.set(p.id, part);
    sensor.addFieldCallback('brickyard', 'isActive', function (active) {
      if (active) {
        part.dragged = null;
        if (selected !== p.id) select(p.id);
      } else if (part.dragged) {
        const t = part.dragged;
        part.dragged = null;
        lastDrag = null;
        if (Math.hypot(t.x, t.y) > 0.5)
          api('move', { id: p.id, dx: (t.x / LDU).toFixed(2), dy: (t.y / LDU).toFixed(2), dz: 0,
                        carry: shift ? 0 : 1 });
        else
          preview(null);
      }
    });
    sensor.addFieldCallback('brickyard', 'translation_changed', function (t) {
      part.dragged = { x: t.x, y: t.y };
      lastDrag = { id: p.id, x: t.x, y: t.y };
      preview(lastDrag);
    });
    return outer;
  }

  // The selected part glows: every Material under it gets an emissive tint.
  function eachMaterial(node, fn) {
    if (!node) return;
    let type = '';
    try { type = node.getNodeTypeName(); } catch (e) { return; }
    if (type === 'Shape') {
      try { if (node.appearance && node.appearance.material) fn(node.appearance.material); } catch (e) {}
      return;
    }
    let children = null;
    try { children = node.children; } catch (e) {}
    if (children) for (let i = 0; i < children.length; i++) eachMaterial(children[i], fn);
  }

  function highlight() {
    parts.forEach(function (part, id) {
      const glow = id === selected ? new X3D.SFColor(0.25, 0.25, 0.1)
        : carried.indexOf(id) >= 0 ? new X3D.SFColor(0.1, 0.1, 0.04) : new X3D.SFColor(0, 0, 0);
      eachMaterial(part.inner, function (m) { m.emissiveColor = glow; });
    });
    document.querySelectorAll('#bom tr').forEach(function (tr) { tr.classList.remove('selected'); });
  }

  function select(id) {
    selected = id;
    carried = [];
    highlight();
    const part = parts.get(id);
    say(part ? part.label + ' (' + id + ')' : '');
    if (part) askCarried(id);
  }

  function report(status) {
    lastStatus = status;
    remember(status.count);
    const lines = [status.count + ' parts, ' + status.joined + ' joined pairs'];
    if (status.collisions.length)
      lines.push(status.collisions.length + ' overlapping: ' + status.collisions.map(function (p) { return p.join('/'); }).join(', '));
    if (status.loose.length)
      lines.push(status.loose.length + ' loose: ' + status.loose.slice(0, 12).join(', ') +
                 (status.loose.length > 12 ? ' and ' + (status.loose.length - 12) + ' more' : ''));
    const name = function (id) { const p = parts.get(id); return (p ? p.label : 'part') + ' (' + id + ')'; };
    if (status.jammed.length)
      lines.push('Jammed: ' + status.jammed.map(name).join(', ') + ' would turn two ways at once');
    const warnings = lines.length;
    if (status.meshes)
      lines.push(status.meshes + (status.meshes === 1 ? ' gear pair meshes' : ' gear pairs mesh'));
    const byMotor = new Map();
    status.drives.forEach(function (d) {
      if (!byMotor.has(d.motor)) byMotor.set(d.motor, []);
      byMotor.get(d.motor).push(name(d.wheel) + (status.jammed.length ? ', but the train is jammed' :
        (d.speed === '1' ? '' : ' at ' + d.speed + ' speed') + (d.reversed ? ', reversed' : '')));
    });
    byMotor.forEach(function (wheels, motor) {
      lines.push(name(motor) + ' drives ' + wheels.join('; '));
    });
    if (status.ground.length)
      lines.push('Stands on ' + status.ground.map(name).join(', '));
    document.getElementById('summary').innerHTML = lines.map(function (l, i) {
      return '<p class="' + (i === 0 || i >= warnings ? '' : 'warn') + '">' + l + '</p>';
    }).join('');
    document.getElementById('bom').innerHTML = status.bom.map(function (row) {
      return '<tr><td>' + row.count + '</td><td><span class="chip" title="' + row.color + '" data-color="' +
        row.color + '"></span></td><td>' + row.label + '</td></tr>';
    }).join('');
    document.querySelectorAll('#bom .chip').forEach(function (chip) {
      const swatch = document.querySelector('.swatch[data-color="' + chip.dataset.color + '"]');
      if (swatch) chip.style.background = swatch.style.background;
    });
  }

  // The parts bin and the swatches.
  document.querySelectorAll('#bin .part').forEach(function (button) {
    button.addEventListener('click', function () {
      const args = { type: button.dataset.type, size: button.dataset.size, color: color };
      const near = selected;
      if (near !== null) args.near = near;
      api('add', args).then(function () {
        const newest = Math.max.apply(null, Array.from(parts.keys()));
        if (!isFinite(newest)) return;
        select(newest);
        // say so when it joined nothing: a newcomer sees it sitting there
        // and cannot tell why Run leaves it be
        if (lastStatus && parts.size > 1 && lastStatus.loose.indexOf(newest) >= 0) {
          const part = parts.get(newest), on = near !== null && parts.get(near);
          say(part.label + ' (' + newest + ') is loose: ' +
              (on ? 'nothing on ' + on.label + ' (' + near + ') takes it.' : 'select the part it should join first.') +
              '  Select an axle with room on it, or drag it into place.');
        }
      });
    });
  });

  document.querySelectorAll('#swatches .swatch').forEach(function (swatch) {
    swatch.addEventListener('click', function () {
      color = swatch.dataset.color;
      document.querySelectorAll('#swatches .swatch').forEach(function (s) { s.classList.toggle('chosen', s === swatch); });
      if (selected !== null) api('color', { id: selected, color: color });
    });
  });

  // Keys act on the selected part.
  const nudges = {
    ArrowLeft: [-20, 0, 0], ArrowRight: [20, 0, 0], ArrowUp: [0, 20, 0], ArrowDown: [0, -20, 0],
    PageUp: [0, 0, 8], PageDown: [0, 0, -8]
  };
  document.addEventListener('keydown', function (e) {
    if (selected === null || e.target.closest('input, textarea')) return;
    const key = e.key, id = selected, carry = e.shiftKey ? 0 : 1;
    if (nudges[key]) {
      const d = nudges[key];
      api('move', { id: id, dx: d[0], dy: d[1], dz: d[2], nudge: 1, carry: carry });
    } else if (key === 'r' || key === 'R') api('turn', { id: id, carry: carry });
    else if (key === 't' || key === 'T') api('tilt', { id: id, carry: carry });
    else if (key === 'e' || key === 'E') api('roll', { id: id, carry: carry });
    // swing about the pin or axle it hangs on: [ ] by 15 degrees, Alt by 5;
    // Shift gives { } on most keyboards, so those count too
    else if (key === '[' || key === '{' || e.code === 'BracketLeft')
      api('rotate', { id: id, degrees: e.altKey ? -5 : -15, carry: carry });
    else if (key === ']' || key === '}' || e.code === 'BracketRight')
      api('rotate', { id: id, degrees: e.altKey ? 5 : 15, carry: carry });
    else if (key === 'c' || key === 'C') api('copy', { id: id }).then(function () {
      select(Math.max.apply(null, Array.from(parts.keys())));
    });
    else if (key === 'Delete' || key === 'Backspace') api('delete', { id: id });
    else if (key === 'Escape') select(null);
    else return;
    e.preventDefault();
  });

  // The prompt box, when the server has one.  A prompt runs on the server
  // and the page asks after it: a model can take longer than a proxy
  // holds a request open.
  const promptForm = document.getElementById('prompt');
  const promptText = document.getElementById('prompt-text');
  const promptGo = document.getElementById('prompt-go');
  const promptUndo = document.getElementById('prompt-undo');
  const promptReply = document.getElementById('prompt-reply');
  const promptModel = document.getElementById('prompt-model');

  // The chosen model is remembered in this browser, when it may remember.
  if (promptModel) {
    try {
      const kept = localStorage.getItem('brickyard-model');
      if (kept && Array.from(promptModel.options).some(function (o) { return o.value === kept; })) promptModel.value = kept;
    } catch (e) {}
    promptModel.addEventListener('change', function () {
      try { localStorage.setItem('brickyard-model', promptModel.value); } catch (e) {}
    });
  }

  function promptCall(args) {
    const body = new URLSearchParams(Object.assign({ b: buildId }, args));
    return fetch('/brickyard/api/prompt', { method: 'POST', body: body }).then(function (r) { return r.json(); });
  }

  function promptBusy(on) {
    promptGo.disabled = on;
    promptText.disabled = on;
    if (promptModel) promptModel.disabled = on;
    promptGo.textContent = on ? 'Building ...' : 'Build';
  }

  function promptSay(text, forms) {
    promptReply.textContent = text || '';
    if (forms && forms.length) {
      const details = document.createElement('details');
      const summary = document.createElement('summary');
      const code = document.createElement('pre');
      summary.textContent = forms.length + (forms.length === 1 ? ' step' : ' steps');
      code.textContent = forms.join('\n');
      details.append(summary, code);
      promptReply.append(details);
    }
    promptReply.hidden = !text && !(forms && forms.length);
  }

  async function promptFinish(reply) {
    promptBusy(false);
    promptSay(reply.reply, reply.forms);
    await apply(reply);
    if ((reply.parts || []).length) browser.viewAll(0.5);
    promptUndo.hidden = false;
  }

  // Ask after the running prompt until it is done; a few failed asks in a
  // row are the network's, more than that and the page stops asking.
  function promptWait(failures) {
    setTimeout(function () {
      promptCall({ action: 'poll' })
        .then(function (reply) {
          if (reply.pending) promptWait(0);
          else if (reply.idle) promptBusy(false);
          else promptFinish(reply);
        })
        .catch(function (e) {
          if (failures < 5) promptWait(failures + 1);
          else { promptBusy(false); say('The server did not answer: ' + e); }
        });
    }, 2000);
  }

  if (promptForm) {
    promptForm.addEventListener('submit', function (e) {
      e.preventDefault();
      const text = promptText.value.trim();
      if (!text || promptGo.disabled) return;
      promptBusy(true);
      promptSay('');
      promptCall({ action: 'start', prompt: text, model: promptModel ? promptModel.value : '' })
        .then(function (reply) {
          if (reply.ok) { promptText.value = ''; promptWait(0); }
          else { promptBusy(false); say(reply.message); }
        })
        .catch(function (err) { promptBusy(false); say('The server did not answer: ' + err); });
    });
    promptText.addEventListener('keydown', function (e) {
      if (e.key === 'Enter' && !e.shiftKey) { e.preventDefault(); promptForm.requestSubmit(); }
    });
    promptUndo.addEventListener('click', function () {
      promptCall({ action: 'undo' })
        .then(function (reply) {
          promptUndo.hidden = true;
          promptSay('');
          return apply(reply);
        })
        .catch(function (err) { say('The server did not answer: ' + err); });
    });
  }

  // Import: the chosen file's text goes to the server, which makes a new
  // build of it and says what it left out; the page then opens that build.
  document.getElementById('import-file').addEventListener('change', function (e) {
    const file = e.target.files[0];
    if (!file) return;
    say('Reading ' + file.name + ' ...');
    file.text()
      .then(function (text) { return fetch('/brickyard/import', { method: 'POST', body: text, headers: { 'Content-Type': 'text/plain' } }); })
      .then(function (r) { return r.json(); })
      .then(function (reply) {
        if (reply.ok) location.href = '/brickyard?b=' + reply.build + '&note=' + encodeURIComponent(reply.note);
        else say(reply.message);
      })
      .catch(function (err) { say('The import did not go through: ' + err); });
    e.target.value = '';
  });

  // Your builds: the ones opened in this browser, newest first, kept in
  // the browser alone (the server keeps each build by its address).
  const yourBuilds = document.getElementById('your-builds');
  function storedBuilds() {
    try { return JSON.parse(localStorage.getItem('brickyard-builds') || '[]'); } catch (e) { return []; }
  }
  function remember(count) {
    const list = storedBuilds().filter(function (b) { return b.id !== buildId; });
    list.unshift({ id: buildId, count: count, at: Date.now() });
    try { localStorage.setItem('brickyard-builds', JSON.stringify(list.slice(0, 20))); } catch (e) {}
    showBuilds();
  }
  function showBuilds() {
    const others = storedBuilds().filter(function (b) { return b.id !== buildId; });
    yourBuilds.hidden = others.length === 0;
    yourBuilds.length = 1;
    others.forEach(function (b) {
      const day = new Date(b.at).toLocaleDateString(undefined, { month: 'short', day: 'numeric' });
      yourBuilds.add(new Option(b.id + ' (' + b.count + ' parts, ' + day + ')', b.id));
    });
  }
  yourBuilds.addEventListener('change', function () {
    if (yourBuilds.value) location.href = '/brickyard?b=' + encodeURIComponent(yourBuilds.value);
  });
  showBuilds();

  // Start here: shown once to a newcomer, and again from the header.
  const welcome = document.getElementById('welcome');
  function showWelcome(shown) { welcome.hidden = !shown; }
  document.getElementById('welcome-open').addEventListener('click', function (e) {
    e.preventDefault();
    showWelcome(welcome.hidden);
  });
  function welcomed() { try { localStorage.setItem('brickyard-welcomed', '1'); } catch (e) {} }
  document.getElementById('welcome-close').addEventListener('click', function () { showWelcome(false); welcomed(); });
  welcome.querySelector('a.button').addEventListener('click', welcomed);
  try { if (!localStorage.getItem('brickyard-welcomed')) showWelcome(true); } catch (e) { showWelcome(true); }

  // Start: the empty world, then every part.
  async function start() {
    browser = X3D.getBrowser(canvas);
    const scene = await browser.createX3DFromString(document.getElementById('scene').textContent);
    await browser.replaceWorld(scene);
    group = browser.currentScene.getNamedNode('parts');
    await api('state');
    browser.viewAll(0);
    if (document.body.dataset.note) say(document.body.dataset.note);
    document.querySelector('#swatches .swatch[data-color="' + color + '"]').classList.add('chosen');
    // a page reloaded while a prompt ran picks it up again
    if (promptForm) promptCall({ action: 'poll' }).then(function (reply) {
      if (reply.pending) { promptBusy(true); promptWait(0); }
      else if (!reply.idle) promptFinish(reply);
    });
  }

  X3D(start, function (e) { say('X_ITE did not start: ' + e); });
})();
