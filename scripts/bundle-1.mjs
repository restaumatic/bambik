// Bundle demo/1: like every other demo it is a named module entered at its
// own function (OrderForm.orderForm), so the entry is synthesized here
// instead of spago bundle-app's fixed Main.main.
import { build } from 'esbuild'

await build({
  stdin: {
    contents: `import { orderForm } from './output/OrderForm/index.js'; orderForm();`,
    resolveDir: process.cwd(),
  },
  bundle: true,
  minify: true,
  format: 'esm',
  outfile: 'demo/1/bundle.js',
})
console.log('bundled 1 (OrderForm.orderForm)')
