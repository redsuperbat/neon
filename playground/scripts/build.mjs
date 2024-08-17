import { within, $, cd } from "zx";

within(async () => {
  cd("../crates/neon_web");
  await $`wasm-pack build --target web`;
});

await $`tsc -b && vite build --base /neon`;
