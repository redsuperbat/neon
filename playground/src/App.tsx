import initScript from "./assets/init.neon?raw";
import init, { CompilationDiagnostics, interpret_src } from "neon-web";
import { Match, Show, Switch, createResource, createSignal } from "solid-js";
import EditorWorker from "monaco-editor/esm/vs/editor/editor.worker?worker";

self.MonacoEnvironment = {
  getWorker: () => new EditorWorker(),
};

import { ProgramSelect } from "./ProgramSelect";
import { Editor } from "./Editor";

function LoadingPage() {
  return <div aria-busy="true" />;
}

function ErrorPage({ error }: { error: string }) {
  return <div>{error}</div>;
}

type Output =
  | {
      type: "error";
      message: string;
    }
  | {
      type: "ok";
      message: string;
    };

function ExecutionPage() {
  const [content, setContent] = createSignal(initScript);
  const [output, setOutput] = createSignal<Output>();

  function exec() {
    const src = content();
    if (!src) return;
    try {
      const res = interpret_src(src);
      setOutput({
        type: "ok",
        message: res.result,
      });
    } catch (e) {
      if (!(e instanceof CompilationDiagnostics)) return;
      setOutput({
        type: "error",
        message: e.errors
          .map((e) => `${e.loc.start.line}:${e.loc.start.col} ${e.message}`)
          .join("\n"),
      });
    }
  }

  return (
    <main
      class="grid h-screen"
      style={{
        "grid-template-rows": "1fr auto",
      }}
    >
      <Editor onContentChange={(s) => setContent(s)} content={content()} />

      <div
        class="grid p-2"
        style={{
          "grid-template-columns": "auto 1fr auto",
        }}
      >
        <ProgramSelect onSelect={(e) => setContent(e.value)} />
        <Show when={output()?.message} fallback={<div />}>
          <div class="flex gap-3">
            <span>Script evaluated to:</span>
            <span
              style={{
                color: output()?.type === "error" ? "red" : "green",
              }}
            >
              {output()?.message}
            </span>
          </div>
        </Show>
        <button type="button" onclick={() => exec()}>
          Execute
        </button>
      </div>
    </main>
  );
}

export function App() {
  const [resource] = createResource(() => init({}));

  return (
    <Switch fallback={<ExecutionPage />}>
      <Match when={resource.loading}>
        <LoadingPage />
      </Match>
      <Match when={resource.error}>
        <ErrorPage error={JSON.stringify(resource.error)} />
      </Match>
    </Switch>
  );
}
