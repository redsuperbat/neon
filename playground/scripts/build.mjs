import { within, $, cd, fs } from "zx";
import pages from "gh-pages";

within(async () => {
  cd("../crates/neon_web");
  await $`wasm-pack build --target web`;
});

await $`tsc -b && vite build`;

await fs.stat("dist");
await new Promise((res) =>
  pages.publish("dist", () => {
    res();
  }),
);
