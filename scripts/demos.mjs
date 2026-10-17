// The demo registry: every demo, its directory under demo/, and the named
// module + entry function it is entered at. Single source of truth shared by
// the bundle scripts and the dev server — no demo is a `Main` module, so
// `spago bundle-app`'s fixed Main.main is never usable and each entry is
// synthesized from this table instead.

// One directory per app: an entry holding several design systems is one page
// that loads the chosen one's bundle (demo/page.js, `?mdc3`). An app's entry
// is the list of its design systems in page order (the first is the page's
// default), and everything else is derived from the app's name: the view
// model module is the app (`Counter`, in `Counter.purs`), each view is the
// app under its design system (`Counter.MDC2`, in `MDC2.purs` — the file is
// named by the design system alone, the module by both, since every demo
// compiles under one `spago build`), and the entry function is the app's
// name (`counter`) in every view. Any further module of the app is named
// the same way, the file its module minus the app prefix (order-dashboard's
// `ControlsMDC3.purs` is `OrderDashboard.ControlsMDC3`). A plain-HTML app
// is one `html` variant (hello-world, restaurant-menu). A law bench is a
// plain [module, fn] pair in a directory of its own.
export const sets = {
  '7guis': {
    'counter': ['mdc2', 'mdc3', 'shoelace', 'fluent', 'bootstrap', 'html'],
    'temperature-converter': ['mdc2', 'mdc3', 'shoelace', 'fluent', 'bootstrap', 'html'],
    'flight-booker': ['mdc2', 'mdc3', 'shoelace', 'fluent', 'bootstrap', 'html'],
    'timer': ['mdc2', 'mdc3', 'shoelace', 'fluent', 'bootstrap', 'html'],
    'crud': ['mdc2', 'mdc3'],
    'circle-drawer': ['mdc2', 'mdc3', 'shoelace', 'fluent', 'bootstrap', 'html'],
    'cells': ['mdc3'],
  },
  'nguis': {
    'order-form': ['mdc2', 'mdc3'],
    'todo-list': ['mdc2', 'mdc3'],
    'tip-calculator': ['mdc2', 'mdc3'],
    'quiz': ['mdc2', 'mdc3'],
    'tic-tac-toe': ['mdc3'],
    'markdown-previewer': ['mdc2', 'mdc3'],
    'hello-world': ['html'],
    'calculator': ['mdc3'],
    'stopwatch': ['mdc2', 'mdc3'],
    'ticker': ['mdc2'],
    'shopping-cart': ['mdc2', 'mdc3'],
    'password-generator': ['mdc2', 'mdc3'],
    'color-mixer': ['mdc3'],
    'signup-form': ['mdc2', 'mdc3'],
    'photo-gallery': ['mdc2', 'mdc3'],
    'inbox': ['mdc2', 'mdc3'],
    'movie-browser': ['mdc2', 'mdc3'],
    'weather': ['mdc2', 'mdc3'],
    'auction': ['mdc2', 'mdc3'],
    'checkout': ['mdc2', 'mdc3'],
    'payment': ['mdc2', 'mdc3'],
    'ticket-dispenser': ['mdc2', 'mdc3'],
    'cashbox': ['mdc2', 'mdc3'],
    'parcel': ['mdc2', 'mdc3'],
    'potluck': ['mdc2', 'mdc3'],
    'departures': ['mdc2', 'mdc3'],
    'scoreboard': ['mdc2', 'mdc3'],
    'reorder': ['mdc2', 'mdc3', 'html'],
    'restaurant-menu': ['html'],
    'espresso-bar': ['mdc3', 'mdc2'],
    'order-dashboard': ['mdc3'],
    'here-and-now': ['mdc3', 'html'],
    'incident-report': ['mdc3'],
    'product-review': ['shoelace'],
    'meeting-booker': ['fluent'],
    'loan-calculator': ['bootstrap'],
  },
  // the leaf-law bench (scripts/smoke/tests/leaf-laws.mjs): every published
  // component mounted alone and fed from outside — not a demo, never deployed
  laws: {
    'mdc2': ['LawBenchMDC2', 'lawBenchMDC2'],
    'mdc3': ['LawBenchMDC3', 'lawBenchMDC3'],
    'shoelace': ['LawBenchShoelace', 'lawBenchShoelace'],
    'fluent': ['LawBenchFluent', 'lawBenchFluent'],
    'bootstrap': ['LawBenchBootstrap', 'lawBenchBootstrap'],
    'html': ['LawBenchHTML', 'lawBenchHTML'],
  },
}

// `dir`, `bundle` and `page` are repo-relative and also the paths served
// under the deployed /bambik/demo/ tree. An entry naming its design systems
// is one directory and one page: each variant bundles to bundle-<variant>.js
// beside the page, which loads the one its query names (`inbox/?mdc3`). A
// plain [mod, fn] entry (a law bench) is a directory of its own with
// bundle.js.

export const labels = { mdc2: 'MDC2', mdc3: 'MDC3', shoelace: 'Shoelace', fluent: 'Fluent', bootstrap: 'Bootstrap', html: 'HTML' }
const pascal = family => family.split('-').map(w => w[0].toUpperCase() + w.slice(1)).join('')
const camel = family => pascal(family).replace(/^./, c => c.toLowerCase())

// Every demo as { set, name, family, variant, mod, fn, file, viewModel,
// viewModelFile, dir, bundle, page }: `mod` and `fn` enter it, `file` is its
// view's source, `viewModel` the module the view determines (null for a law
// bench; hello-world, all view, has the name and no file) and
// `viewModelFile` that module's source.
export const all = Object.entries(sets).flatMap(([set, demos]) =>
  Object.entries(demos).flatMap(([family, entry]) => {
    const dir = `demo/${set}/${family}`
    const app = pascal(family)
    if (Array.isArray(entry) && entry.every(v => v in labels)) return entry.map(variant => ({
      set, name: `${family}-${variant}`, family, variant, mod: `${app}.${labels[variant]}`, fn: camel(family),
      file: `${dir}/${labels[variant]}.purs`, viewModel: app, viewModelFile: `${dir}/${app}.purs`, dir,
      bundle: `${dir}/bundle-${variant}.js`, page: `${dir}/?${variant}`,
    }))
    const [mod, fn] = entry
    return [{ set, name: family, family, variant: null, mod, fn, file: `${dir}/${mod}.purs`, viewModel: null, viewModelFile: null,
      dir, bundle: `${dir}/bundle.js`, page: `${dir}/` }]
  }))

export const viewModelModules = new Set(all.map(d => d.viewModel).filter(Boolean))
export const viewModelFiles = new Set(all.map(d => d.viewModelFile).filter(Boolean))

export const inSet = set => all.filter(d => d.set === set)

// a filter names a demo, an app (every design system of it) or a set
export const matching = filters => filters.length
  ? all.filter(d => filters.some(f => f === d.name || f === d.family || f === d.set))
  : all

export const byName = name => {
  const d = all.find(d => d.name === name)
  if (!d) throw new Error(`no demo named ${name} in scripts/demos.mjs`)
  return d
}

export const entryFor = ({ mod, fn }) =>
  `import { ${fn} } from './output/${mod}/index.js'; ${fn}();`
