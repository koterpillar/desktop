import { View, div, svg } from "gpui-kit";
import { h_flex, v_flex } from "gpui-base";
import { checking, exec, hc, layout, panes, settings, spaces, theme, window } from "herdr";

// Homebrew and Linux ship different versions of pstree.
const PSTREE = `
  command -v pstree >/dev/null || { echo 'Install pstree on the terminal machine.' >&2; exit 127; }
  platform="$(uname -s)"
  for pid in "$@"; do
    if [ "$platform" = Darwin ]; then
      pstree -g 3 -w "$pid" || exit
    elif [ -d "/proc/$pid" ]; then
      pstree -a -c -l -p -T -U "$pid" || exit
    fi
  done
`;

function parseTree(text) {
  const roots = [], ancestors = [];
  for (const line of text.split("\n")) {
    const mac = /^([ │]*)[├└]?[─┬=]+\s+(\d+)\s+\S+\s+(.*)$/.exec(line);
    const linux = !mac && /^([ │]*)(?:[├└]─)?(.+?),(\d+)\)?(?= |$) *(.*)$/.exec(line);
    if (!mac && !linux) continue;

    const column = (mac || linux)[1].length;
    const pid = Number(mac ? mac[2] : linux[3]);
    const executable = mac ? /^(?:"([^"]+)"|'([^']+)'|(\S+))/.exec(mac[3])?.slice(1).find(Boolean) : linux[2].replace(/^\(/, "");
    const command = mac ? mac[3] : [executable, linux[4]].filter(Boolean).join(" ");
    const name = executable?.split("/").pop()?.replace(/^-/, "") || command || "Unknown";
    // The branch column increases with depth, including for one-letter names in psmisc.
    while (ancestors.length && ancestors.at(-1).column >= column) ancestors.pop();
    const parent = ancestors.at(-1);
    const process = { id: parent ? parent.id + "/" + pid : String(pid), name, pid, command, column, children: [] };
    (parent?.children ?? roots).push(process);
    ancestors.push(process);
  }
  return roots;
}

export default class Processes extends View {
  init(_props, cx) {
    if (!checking()) cx.timer.every(2000, (cx) => this.refresh(cx));
  }

  refresh(cx) {
    const panel = layout()?.children?.find((node) => node.id === "work")
      ?.children.find((node) => node.id === "work-body")
      ?.children.find((node) => node.id === "panel");
    if (!panel || panel.hidden || panel.children.find((node) => node.id === "panel-body")?.active !== "processes"
      || !window()?.visible) {
      this.snapshot = null;
      return;
    }

    const space = spaces().find((space) => space.active);
    const tab = space?.tabs.find((tab) => tab.active)?.id;
    const terminals = [...new Set(panes().filter((pane) => pane.tab === tab && pane.terminal).map((pane) => pane.terminal))];
    const key = JSON.stringify([tab, terminals]);
    if (this.snapshot?.key !== key) {
      this.snapshot = { key, collapsed: new Set(), message: terminals.length ? "Loading processes…" : "No running processes" };
    }
    const snapshot = this.snapshot;
    if (!terminals.length || snapshot.loading || Date.now() < snapshot.next) return;

    snapshot.loading = true;
    cx.spawn(async (cx) => {
      let output = "", error = "";
      try {
        const pids = await Promise.all(terminals.map(async (terminal) => {
          const result = await hc(["get", terminal, "--json"]);
          if (result.code !== 0) throw new Error("Could not read terminal details");
          return JSON.parse(result.stdout).pid;
        }));
        if (this.snapshot !== snapshot) return;
        const roots = [...new Set(pids)].filter((pid) => Number.isInteger(pid) && pid > 0);
        if (roots.length) {
          const result = await exec("sh", ["-c", PSTREE, "pstree", ...roots.map(String)], { on: space.machine, timeout: 5000 });
          if (result.code !== 0) throw new Error(result.stderr.trim() || "pstree failed");
          output = result.stdout.trimEnd();
        }
      } catch (cause) {
        error = cause.message || "Could not read processes";
      }
      if (this.snapshot !== snapshot) return;
      snapshot.loading = false;
      snapshot.next = Date.now() + 1500;
      if (snapshot.output !== output || snapshot.error !== error) {
        snapshot.output = output;
        snapshot.error = error;
        snapshot.roots = parseTree(output);
        snapshot.message = error || (output ? "Could not read the process tree" : "No running processes");
        cx.notify();
      }
    });
  }

  branch(process, t, font, depth = 0) {
    const { id, name, pid, command, children } = process;
    const snapshot = this.snapshot;
    const open = !snapshot.collapsed.has(id);
    const selected = snapshot.selected === id;
    const icon = children.length
      ? svg("mods/workspace-chrome/chevron-" + (open ? "down" : "right") + ".svg").size(13)
      : depth ? div().size(4).rounded(2).bg(t.dim) : svg("mods/workspace-chrome/terminal.svg").size(13);
    const disclosure = div().id("branch-" + id).size(23).flex_none().flex().items_center().justify_center().rounded(4)
      .text_color(t.muted).child(icon)
      .when(children.length > 0, (el) => el.cursor_pointer().hover((s) => s.bg(t.hover).text_color(t.foreground))
        .on_click((_e, cx) => {
          if (open) snapshot.collapsed.add(id);
          else snapshot.collapsed.delete(id);
          cx.notify();
        }));
    return v_flex().min_w(0).flex_none().gap(2)
      .child(h_flex().min_w(0).h(28).gap(3).items_center().pl(2).pr(7).rounded(5)
        .when(depth === 0, (el) => el.bg(t.raised))
        .when(selected, (el) => el.bg(t.selected))
        .child(disclosure)
        .child(h_flex().id("process-" + id).flex_1().min_w(0).h_full().gap(8).items_center().cursor_pointer()
          .hover((s) => s.text_color(t.accent))
          .on_click((_e, cx) => {
            snapshot.selected = selected ? null : id;
            cx.notify();
          })
          .child(div().flex_1().min_w(0).truncate().when(depth === 0, (el) => el.font_semibold()).child(name))
          .child(div().flex_none().text_right().font_family(font).text_size(11).text_color(t.muted).child(String(pid)))))
      .when(selected, (el) => el.child(div().id("command-" + id).ml(25).min_w(0).rounded(5).bg(t.raised)
        .font_family(font).text_size(11).text_color(t.muted).overflow_x_scrollbar()
        .child(div().px(8).py(6).whitespace_nowrap().child(command))))
      .when(open && children.length > 0, (el) => el.child(v_flex().min_w(0).gap(2)
        .when(depth < 7, (group) => group.ml(13).pl(7).border_l(1).border_color(t.border))
        .children(children.map((child) => this.branch(child, t, font, depth + 1)))));
  }

  render(cx) {
    if (!checking()) this.refresh(cx);
    const t = theme();
    const font = settings().font_family ?? "JetBrains Mono";
    const roots = this.snapshot?.roots ?? [];
    return v_flex().id("processes").size_full().min_h(0).rounded(10).overflow_hidden()
      .bg(t.background).border_1().border_color(t.border).text_color(t.foreground).text_size(13)
      .child(h_flex().h(42).flex_none().px(14).items_center().border_b(1).border_color(t.border)
        .child(div().flex_1().font_semibold().child("Processes"))
        .when(roots.length > 0, (el) => el.child(div().text_size(11).text_color(t.muted).child("PID"))))
      .child(div().flex_1().min_h(0).relative()
        .child(v_flex().id("process-tree").absolute().inset_0().overflow_y_scroll().p(7).gap(8)
          .children(roots.map((root) => this.branch(root, t, font)))
          .when(!roots.length, (el) => el.child(div().px(7).py(8).text_color(t.muted)
            .child(this.snapshot?.message ?? "Loading processes…")))));
  }
}
