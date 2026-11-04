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

  async function apply(reply) {
    say(reply.ok ? '' : reply.message);
    (reply.removed || []).forEach(removePart);
    for (const p of reply.parts || []) await putPart(p);
    if (reply.status) report(reply.status);
    if (selected !== null && !parts.has(selected)) selected = null;
    highlight();
    if (selected !== null && ((reply.parts || []).length || (reply.removed || []).length)) askCarried(selected);
  }

  function removePart(id) {
    const part = parts.get(id);
    if (!part) return;
    group.removeChildren = new X3D.MFNode(part.outer);
    parts.delete(id);
  }

  async function putPart(p) {
    const scene = await browser.createX3DFromString(p.x3d);
    const outer = scene.getNamedNode('p' + p.id);
    const sensor = scene.getNamedNode('p' + p.id + 's');
    const inner = scene.getNamedNode('p' + p.id + 'd');
    removePart(p.id);
    group.addChildren = new X3D.MFNode(outer);
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
    const lines = [status.count + ' parts, ' + status.joined + ' joined pairs'];
    if (status.collisions.length)
      lines.push(status.collisions.length + ' overlapping: ' + status.collisions.map(function (p) { return p.join('/'); }).join(', '));
    if (status.loose.length)
      lines.push(status.loose.length + ' loose: ' + status.loose.join(', '));
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
      if (selected !== null) args.near = selected;
      api('add', args).then(function () {
        const newest = Math.max.apply(null, Array.from(parts.keys()));
        if (isFinite(newest)) select(newest);
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
      api('move', { id: id, dx: d[0], dy: d[1], dz: d[2], exact: 1, carry: carry });
    } else if (key === 'r' || key === 'R') api('turn', { id: id, carry: carry });
    else if (key === 't' || key === 'T') api('tilt', { id: id, carry: carry });
    else if (key === 'e' || key === 'E') api('roll', { id: id, carry: carry });
    else if (key === 'c' || key === 'C') api('copy', { id: id }).then(function () {
      select(Math.max.apply(null, Array.from(parts.keys())));
    });
    else if (key === 'Delete' || key === 'Backspace') api('delete', { id: id });
    else if (key === 'Escape') select(null);
    else return;
    e.preventDefault();
  });

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
  }

  X3D(start, function (e) { say('X_ITE did not start: ' + e); });
})();
