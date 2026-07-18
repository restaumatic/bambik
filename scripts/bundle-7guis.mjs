// Bundle every 7GUIs demo: each page is module <Name> entered at its own
// named function (counter, timer, ...), so the entry is synthesized here
// instead of spago bundle-app's fixed Main.main.
import { build } from 'esbuild'

const demos = {
  'counter': ['Counter', 'counter'],
  'temperature-converter': ['TemperatureConverter', 'temperatureConverter'],
  'flight-booker': ['FlightBooker', 'flightBooker'],
  'timer': ['Timer', 'timer'],
  'crud': ['Crud', 'crud'],
  'circle-drawer': ['CircleDrawer', 'circleDrawer'],
  'cells': ['Cells', 'cells'],
}

for (const [dir, [mod, fn]] of Object.entries(demos)) {
  await build({
    stdin: {
      contents: `import { ${fn} } from './output/${mod}/index.js'; ${fn}();`,
      resolveDir: process.cwd(),
    },
    bundle: true,
    minify: true,
    format: 'esm',
    outfile: `demo/7guis/${dir}/bundle.js`,
  })
  console.log(`bundled ${dir} (${mod}.${fn})`)
}
