import { within, $, cd } from "zx";

await within(async () => {
  cd("../crates/neon_web");
  await $`wasm-pack build --target web`;
});
