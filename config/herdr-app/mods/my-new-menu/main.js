import { View, div, svg, image } from "gpui-kit";
import { h_flex, v_flex } from "gpui-base";
import { theme, spaces, panes, knownAgents, run, openOverlay, closeOverlay, props, window, keys, shared, setShared } from "herdr";

// The last agents each machine reported, shown at once while knownAgents() checks again.
const KNOWN = "new-menu.agents";

function shortcut(name) {
  const k = keys().find(b => b.section === "global" && b.action === name);
  if (!k?.keys.trim()) return "";
  const mac = window()?.platform === "macos";
  const names = mac ? { ctrl: "⌃", alt: "⌥", shift: "⇧", cmd: "⌘", fn: "fn" } : { ctrl: "Ctrl", alt: "Alt", shift: "Shift", cmd: "Super", fn: "Fn" };
  const alias = { secondary: mac ? "cmd" : "ctrl", super: "cmd", win: "cmd" };
  return k.keys.trim().split(/\s+/).map(stroke => {
    const parts = stroke.split("-");
    let key = parts.pop();
    if (key === "") { key = "-"; parts.pop(); }
    const held = new Set(parts.map(m => alias[m.toLowerCase()] ?? m.toLowerCase()));
    if (/^[A-Z]$/.test(key)) held.add("shift");
    const label = key.length === 1 ? key.toUpperCase() : key[0].toUpperCase() + key.slice(1).toLowerCase();
    return [...Object.keys(names).filter(m => held.has(m)).map(m => names[m]), label].join(mac ? "" : "+");
  }).join(" ");
}

export default class NewMenu extends View {
  init(_props, cx) {
    const machine = spaces().find(s => s.active)?.machine;
    const where = machine ?? "";
    this.agents = shared(KNOWN)?.[where] ?? [];
    this.handle = cx.focus_handle();
    this.handle.focus();
    cx.spawn(async cx => {
      try {
        this.agents = await knownAgents({ on: machine });
        setShared(KNOWN, { ...shared(KNOWN), [where]: this.agents });
      }
      catch { this.failed = true; }
      cx.notify();
    });
  }
  // Rows are plain data, so the keys and the clicks run the same thing.
  row(it, on) {
    const t = theme();
    return h_flex().id(it.id).h(31).px(9).gap(10).rounded(7).items_center().flex_none()
      .text_color(it.enabled ? t.foreground : t.dim)
      .when(on, el => el.bg(t.selected))
      .when(it.enabled, el => el.hover(s => s.bg(t.selected)))
      .on_click(() => this.pick(it))
      .child(it.icon).child(div().flex_1().child(it.name)).child(div().text_size(11).text_color(t.dim).child(it.hint ?? ""));
  }
  pick(it) {
    if (!it?.enabled) return;
    closeOverlay(props().overlay);
    it.action();
  }
  render() {
    const t = theme();
    const space = spaces().find(s => s.active);
    const installed = this.agents.filter(a => a.installed);
    const choices = [...installed, ...this.agents.filter(a => !a.installed).slice(0, 2)];
    const folder = () => panes().find(p => p.focused)?.cwd;
    const terminal = { id: "terminal", icon: svg("icons/shell.svg").size(14), name: "Terminal", action: () => run("tab.new", { space: space?.id }), hint: shortcut("tab.new"), enabled: true };
    const agentRows = choices.map((a, i) => ({ id: a.key, icon: a.tint ? svg(a.icon).size(14).text_color(t.foreground) : image(a.icon).size(14), name: a.name,
      action: () => run("tab.new", { name: a.name, space: space?.id, argv: a.run }), hint: a.installed ? (i < 9 ? String(i + 1) : "") : "not installed", enabled: a.installed }));
    const tail = [
      { id: "space", icon: div().w(14).child("+"), name: "New space", action: () => run("space.new"), hint: shortcut("space.new"), enabled: true },
      { id: "worktree", icon: svg("mods/workspace-chrome/branch.svg").size(14), name: "New worktree\u2026", action: () => openOverlay({ mod: "worktree", kind: "dialog", width: 460, height: 280, props: { space: space?.id, folder: folder(), action: "new" } }), hint: "W", enabled: Boolean(space) },
      { id: "worktree-open", icon: svg("mods/workspace-chrome/branch.svg").size(14), name: "Open worktree\u2026", action: () => openOverlay({ mod: "worktree", kind: "dialog", width: 560, height: 480, props: { space: space?.id, folder: folder(), action: "open" } }), enabled: Boolean(space) },
    ];
    // Up/down/Enter move over the rows that can be chosen; it opens on Terminal.
    const pickable = [terminal, ...agentRows, ...tail].filter(it => it.enabled);
    this.at = Math.min(this.at ?? 0, pickable.length - 1);
    const sel = pickable[this.at];
    const line = it => this.row(it, it === sel);
    return v_flex().min_h(0).w_full().h(Math.min(491, 167 + Math.max(1, choices.length) * 31)).bg(t.popover).p(5).text_size(12.8)
      .id("new-menu").track_focus(this.handle)
      .on_key_down((e, cx) => {
        if (e.modifiers.control || e.modifiers.platform || e.modifiers.alt) return;
        const n = pickable.length;
        if (e.key === "down") this.at = (this.at + 1) % n;
        else if (e.key === "up") this.at = (this.at + n - 1) % n;
        else if (e.key === "enter" || e.key === "return") { cx.stop_propagation(); return this.pick(sel); }
        else if (e.key.toLowerCase() === "w") { cx.stop_propagation(); return this.pick(tail.find(it => it.id === "worktree")); }
        else if (/^[1-9]$/.test(e.key) && installed[Number(e.key) - 1]) {
          const agent = installed[Number(e.key) - 1];
          closeOverlay(props().overlay);
          return run("tab.new", { name: agent.name, space: space?.id, argv: agent.run });
        } else return;
        cx.stop_propagation();
        cx.notify();
      })
      .child(line(terminal))
      .child(div().px(9).pt(8).pb(4).text_size(11).text_color(t.dim).child("Start an agent"))
      .child(v_flex().flex_1().min_h(0).overflow_y_scrollbar()
        .children(agentRows.map(line))
        .when(this.agents.length === 0, el => el.child(div().px(9).py(6).text_color(t.dim).child(this.failed ? "Could not load agents" : "Checking installed agents…"))))
      .child(div().h(1).mx(4).my(5).bg(t.border))
      .children(tail.map(line));
  }
}
