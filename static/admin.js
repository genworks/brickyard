// Copyright © 2026 Genworks International
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.  Distributed WITHOUT
// ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.
//
// The admin page, /brickyard/admin (source/admin.lisp): the visitors
// with what each did, build by build, and the builds in memory with
// their prompts and the model's answers.  It reads, and deletes from the
// activity log what its operator asks it to, each time after asking
// back: one line, or everything kept of a visitor.  Everything shown
// here that a visitor typed -- a prompt, a program, a request's body --
// goes into the page as text, never as markup.  The pieces it is built
// from (elements, fetching, times, pills, the delete button) are
// window.visitorAdmin, Gendl's visitor-log's, which the page loads first.

(function () {
  'use strict';

  var root = document.getElementById('admin');
  var opened = {};   // visitor key -> true while that visitor's card is open
  var limits = {};   // visitor key -> how many lines to ask for
  var EVENTS = 300;

  // What every operator's page is built from, served by
  // /brickyard/admin-kit.js (Gendl's visitor-log).
  var A = window.visitorAdmin;
  var el = A.el, fill = A.fill, get = A.get, when = A.when, ago = A.ago, count = A.count,
      pill = A.pill, fact = A.fact, more = A.more;

  function buildLink(id) {
    return el('a', { href: '/brickyard?b=' + encodeURIComponent(id), target: '_blank', rel: 'noopener',
                     'class': 'ad-mono', text: id });
  }

  // A button that asks before it deletes, then reloads what it shows.
  function deleter(label, question, kind, body, after) {
    var b = A.remover('/brickyard/api/admin/delete', label, kind, question, body, after);
    b.setAttribute('title', question);
    return b;
  }

  // ---- one visitor's record -------------------------------------------------

  var LABELS = { visit: 'visit', change: 'change', table: 'table', prompt: 'prompt',
                 download: 'export', 'import': 'import' };

  function what(v, ev) {
    var on = ev.build ? [el('span', { 'class': 'ad-dim', text: ' on ' }), buildLink(ev.build)] : [];
    if (ev.kind === 'visit') {
      return [ev.op ? 'Made a new build (' + ev.op + ')' : 'Opened the page'].concat(on);
    }
    if (ev.kind === 'download') return ['Exported LDraw'].concat(on);
    if (ev.kind === 'import') {
      var src = '/brickyard/api/admin/file?who=' + encodeURIComponent(v.key) + '&file=' + encodeURIComponent(ev.file || '');
      return ['Imported an LDraw file, ', String(Math.round((ev.bytes || 0) / 1024)) + ' KB ',
              ev.file ? el('a', { href: src, target: '_blank', rel: 'noopener', text: 'the file' }) : null];
    }
    if (ev.kind === 'prompt') {
      if (ev.op === 'undo') return ['Undid the last prompt'].concat(on);
      return [el('div', { 'class': 'ad-words', text: ev.prompt || '' }),
              ev.model ? pill(ev.model, 'model') : null].concat(on);
    }
    if (ev.kind === 'table') {
      return [ev.op === 'python' ? 'Wrote the program out for Pybricks' : 'Ran a program on the table'].concat(on)
        .concat([ev.program ? more('the program', ev.program) : null]);
    }
    return [el('span', { 'class': 'ad-mono', text: ev.op || ev.door })].concat(on)
      .concat([ev.body ? more('the request', ev.body) : null]);
  }

  function lineRow(v, ev, reload) {
    var row = A.eventRow(ev.time, LABELS[ev.kind] || ev.kind, '', what(v, ev));
    row.appendChild(deleter('×', 'Delete this line from ' + (v.name || v.key) + '\'s record?', 'del',
                            { who: v.key, line: ev.line, mark: ev.mark }, reload));
    return row;
  }

  function loadVisitor(v, body) {
    var limit = limits[v.key] || EVENTS;
    fill(body, ['Loading...']);
    get('/brickyard/api/admin/visitor?who=' + encodeURIComponent(v.key) + '&limit=' + limit).then(function (r) {
      var reload = function () { loadVisitor(v, body); refresh(); };
      var kids = [];
      if (r.total > r.events.length) {
        var older = el('button', { type: 'button', text: 'Show ' + Math.min(r.total - r.events.length, limit) + ' older' });
        older.addEventListener('click', function () { limits[v.key] = limit * 2; loadVisitor(v, body); });
        kids.push(el('p', { 'class': 'ad-dim' }, ['The last ' + r.events.length + ' of ' + r.total + ' lines. ', older]));
      }
      if (!r.events.length) kids.push(el('p', { 'class': 'ad-dim', text: 'Nothing kept.' }));
      r.events.forEach(function (ev) { kids.push(lineRow(v, ev, reload)); });
      if (r.total) {
        kids.push(el('p', {}, [deleter('Delete everything kept of ' + (v.name || v.key),
                                       'Delete all ' + count(r.total, 'line') + ' kept of ' + (v.name || v.key) +
                                       ', and any file they imported?  Their invite stays.',
                                       'danger', { who: v.key, everything: true }, reload)]));
      }
      fill(body, kids);
    }, function (e) {
      fill(body, [el('p', { 'class': 'ad-warn', text: e.message })]);
    });
  }

  function visitorCard(v) {
    var counts = [];
    [['visits', 'visit'], ['changes', 'change'], ['runs', 'run'], ['prompts', 'prompt'],
     ['downloads', 'export'], ['imports', 'import']].forEach(function (p) {
      if (v[p[0]]) counts.push(count(v[p[0]], p[1]));
    });
    var summary = el('summary', {}, [
      el('span', { 'class': 'ad-name', text: v.name || v.key }),
      v.invited ? null : pill('no invite'),
      el('span', { 'class': 'ad-dim', text: counts.join(', ') || 'nothing recorded' }),
      v.last ? el('span', { 'class': 'ad-dim', title: v.last, text: 'last ' + ago(v.last) }) : null,
      v.invited ? el('span', { 'class': 'ad-dim', text: 'invited ' + (v.issued || '') + ', link used ' +
                               count(v.uses || 0, 'time') + (v.seen ? ', last ' + ago(v.seen) : '') }) : null]);
    var builds = v.builds && v.builds.length
      ? el('p', { 'class': 'ad-dim' }, ['Builds: '].concat(v.builds.reduce(function (acc, b, i) {
          if (i) acc.push(', ');
          acc.push(buildLink(b.id));
          acc.push(' (' + count(b.requests, 'request') + ')');
          return acc;
        }, [])))
      : null;
    var body = el('div', {});
    var card = el('details', { 'class': 'ad-card' }, [summary, el('div', { 'class': 'ad-body' }, [builds, body])]);
    if (opened[v.key]) { card.open = true; loadVisitor(v, body); }
    card.addEventListener('toggle', function () {
      opened[v.key] = card.open;
      if (card.open) loadVisitor(v, body);
    });
    return card;
  }

  // ---- the builds in memory -------------------------------------------------

  function buildCard(b) {
    var summary = el('summary', {}, [
      buildLink(b.id),
      el('span', { 'class': 'ad-dim', text: count(b.parts, 'part') }),
      b.running ? pill('prompt running', 'live') : null,
      b.turns.length ? el('span', { 'class': 'ad-dim', text: count(b.turns.length, 'prompt') }) : null,
      b.who ? el('span', { 'class': 'ad-dim', text: 'by ' + b.who + (b.last ? ', ' + ago(b.last) : '') }) : null]);
    var turns = b.turns.map(function (t) {
      return el('div', {}, [el('div', { 'class': 'ad-words', text: t.prompt || '' }),
                            el('div', { 'class': 'ad-reply ad-mono', text: t.reply || '' })]);
    });
    if (!turns.length) turns.push(el('p', { 'class': 'ad-dim', text: 'No prompts in memory.' }));
    return el('details', { 'class': 'ad-card' }, [summary, el('div', { 'class': 'ad-body' }, turns)]);
  }

  // ---- the page -------------------------------------------------------------

  function refresh() {
    return get('/brickyard/api/admin').then(draw, function (e) {
      fill(root, [el('p', { 'class': 'ad-warn', text: 'The summary did not load: ' + e.message })]);
    });
  }

  function draw(s) {
    var server = s.server;
    var again = el('button', { type: 'button', text: 'Refresh' });
    again.addEventListener('click', refresh);
    var facts = el('div', { 'class': 'ad-facts' }, [
      fact('at', when(s.now)),
      fact('Lisp', server.lisp),
      server.heap === null ? null : fact('heap', Math.round(server.heap * 100) + '%', server.heap > 0.7 ? 'warn' : ''),
      fact('gate', server.gate ? 'on' : 'open', server.gate ? '' : 'warn'),
      fact('activity log', server.activity ? 'on' : 'off', server.activity ? '' : 'warn'),
      fact('builds in memory', server.builds),
      fact('models', server.models.length ? server.models.join(', ') : 'none')]);
    var visitors = s.visitors.length
      ? s.visitors.map(visitorCard)
      : [el('p', { 'class': 'ad-dim', text: 'No one yet.' })];
    var builds = s.builds.length
      ? s.builds.slice(0, 200).map(buildCard)
      : [el('p', { 'class': 'ad-dim', text: 'None.' })];
    fill(root, [el('div', { 'class': 'ad-head' }, [el('h1', { text: 'Brickyard admin' }), again]),
                facts,
                el('h2', { text: 'Visitors' })].concat(visitors)
               .concat([el('h2', { text: 'Builds in memory' })])
               .concat(s.builds.length > 200 ? [el('p', { 'class': 'ad-dim', text: 'The 200 most recent of ' + s.builds.length + '.' })] : [])
               .concat(builds));
  }

  refresh();
})();
