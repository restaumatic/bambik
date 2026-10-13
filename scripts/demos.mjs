// The demo registry: every demo, its directory under demo/, and the named
// module + entry function it is entered at. Single source of truth shared by
// the bundle scripts and the dev server — no demo is a `Main` module, so
// `spago bundle-app`'s fixed Main.main is never usable and each entry is
// synthesized from this table instead.

// One directory per app: an entry holding several design systems is one page
// that loads the chosen one's bundle (demo/page.js, `?mdc3`).
export const sets = {
  '7guis': {
    'counter': {
      mdc2: ['CounterMDC2', 'counterMDC2'],
      mdc3: ['CounterMDC3', 'counterMDC3'],
      shoelace: ['CounterShoelace', 'counterShoelace'],
      fluent: ['CounterFluent', 'counterFluent'],
      bootstrap: ['CounterBootstrap', 'counterBootstrap'],
      html: ['CounterHTML', 'counterHTML'],
    },
    'temperature-converter': {
      mdc2: ['TemperatureConverterMDC2', 'temperatureConverterMDC2'],
      mdc3: ['TemperatureConverterMDC3', 'temperatureConverterMDC3'],
      shoelace: ['TemperatureConverterShoelace', 'temperatureConverterShoelace'],
      fluent: ['TemperatureConverterFluent', 'temperatureConverterFluent'],
      bootstrap: ['TemperatureConverterBootstrap', 'temperatureConverterBootstrap'],
      html: ['TemperatureConverterHTML', 'temperatureConverterHTML'],
    },
    'flight-booker': {
      mdc2: ['FlightBookerMDC2', 'flightBookerMDC2'],
      mdc3: ['FlightBookerMDC3', 'flightBookerMDC3'],
      shoelace: ['FlightBookerShoelace', 'flightBookerShoelace'],
      fluent: ['FlightBookerFluent', 'flightBookerFluent'],
      bootstrap: ['FlightBookerBootstrap', 'flightBookerBootstrap'],
      html: ['FlightBookerHTML', 'flightBookerHTML'],
    },
    'timer': {
      mdc2: ['TimerMDC2', 'timerMDC2'],
      mdc3: ['TimerMDC3', 'timerMDC3'],
      shoelace: ['TimerShoelace', 'timerShoelace'],
      fluent: ['TimerFluent', 'timerFluent'],
      bootstrap: ['TimerBootstrap', 'timerBootstrap'],
      html: ['TimerHTML', 'timerHTML'],
    },
    'crud': {
      mdc2: ['CrudMDC2', 'crudMDC2'],
      mdc3: ['CrudMDC3', 'crudMDC3'],
      shoelace: ['CrudShoelace', 'crudShoelace'],
      fluent: ['CrudFluent', 'crudFluent'],
      bootstrap: ['CrudBootstrap', 'crudBootstrap'],
      html: ['CrudHTML', 'crudHTML'],
    },
    'circle-drawer': {
      mdc2: ['CircleDrawerMDC2', 'circleDrawerMDC2'],
      mdc3: ['CircleDrawerMDC3', 'circleDrawerMDC3'],
      shoelace: ['CircleDrawerShoelace', 'circleDrawerShoelace'],
      fluent: ['CircleDrawerFluent', 'circleDrawerFluent'],
      bootstrap: ['CircleDrawerBootstrap', 'circleDrawerBootstrap'],
      html: ['CircleDrawerHTML', 'circleDrawerHTML'],
    },
    'cells': {
      mdc2: ['CellsMDC2', 'cellsMDC2'],
      mdc3: ['CellsMDC3', 'cellsMDC3'],
      shoelace: ['CellsShoelace', 'cellsShoelace'],
      fluent: ['CellsFluent', 'cellsFluent'],
      bootstrap: ['CellsBootstrap', 'cellsBootstrap'],
      html: ['CellsHTML', 'cellsHTML'],
    },
  },
  'nguis': {
    'order-form': { mdc2: ['OrderFormMDC2', 'orderFormMDC2'], mdc3: ['OrderFormMDC3', 'orderFormMDC3'] },
    'todo-list': { mdc2: ['TodoListMDC2', 'todoListMDC2'], mdc3: ['TodoListMDC3', 'todoListMDC3'] },
    'tip-calculator': { mdc2: ['TipCalculatorMDC2', 'tipCalculatorMDC2'], mdc3: ['TipCalculatorMDC3', 'tipCalculatorMDC3'] },
    'quiz': { mdc2: ['QuizMDC2', 'quizMDC2'], mdc3: ['QuizMDC3', 'quizMDC3'] },
    'tic-tac-toe': { mdc2: ['TicTacToeMDC2', 'ticTacToeMDC2'], mdc3: ['TicTacToeMDC3', 'ticTacToeMDC3'] },
    'markdown-previewer': { mdc2: ['MarkdownPreviewerMDC2', 'markdownPreviewerMDC2'], mdc3: ['MarkdownPreviewerMDC3', 'markdownPreviewerMDC3'] },
    'helloworld': ['HelloWorld', 'helloWorld'],
    'calculator': { mdc2: ['CalculatorMDC2', 'calculatorMDC2'], mdc3: ['CalculatorMDC3', 'calculatorMDC3'] },
    'stopwatch': { mdc2: ['StopwatchMDC2', 'stopwatchMDC2'], mdc3: ['StopwatchMDC3', 'stopwatchMDC3'] },
    'ticker': { mdc2: ['TickerMDC2', 'tickerMDC2'] },
    'shopping-cart': { mdc2: ['ShoppingCartMDC2', 'shoppingCartMDC2'], mdc3: ['ShoppingCartMDC3', 'shoppingCartMDC3'] },
    'password-generator': { mdc2: ['PasswordGeneratorMDC2', 'passwordGeneratorMDC2'], mdc3: ['PasswordGeneratorMDC3', 'passwordGeneratorMDC3'] },
    'color-mixer': { mdc2: ['ColorMixerMDC2', 'colorMixerMDC2'], mdc3: ['ColorMixerMDC3', 'colorMixerMDC3'] },
    'signup-form': { mdc2: ['SignupFormMDC2', 'signupFormMDC2'], mdc3: ['SignupFormMDC3', 'signupFormMDC3'] },
    'photo-gallery': { mdc2: ['PhotoGalleryMDC2', 'photoGalleryMDC2'], mdc3: ['PhotoGalleryMDC3', 'photoGalleryMDC3'] },
    'inbox': { mdc2: ['InboxMDC2', 'inboxMDC2'], mdc3: ['InboxMDC3', 'inboxMDC3'] },
    'movie-browser': { mdc2: ['MovieBrowserMDC2', 'movieBrowserMDC2'], mdc3: ['MovieBrowserMDC3', 'movieBrowserMDC3'] },
    'weather': { mdc2: ['WeatherMDC2', 'weatherMDC2'], mdc3: ['WeatherMDC3', 'weatherMDC3'] },
    'auction': { mdc2: ['AuctionMDC2', 'auctionMDC2'], mdc3: ['AuctionMDC3', 'auctionMDC3'] },
    'checkout': { mdc2: ['CheckoutMDC2', 'checkoutMDC2'], mdc3: ['CheckoutMDC3', 'checkoutMDC3'] },
    'payment': { mdc2: ['PaymentMDC2', 'paymentMDC2'], mdc3: ['PaymentMDC3', 'paymentMDC3'] },
    'ticket-dispenser': { mdc2: ['TicketDispenserMDC2', 'ticketDispenserMDC2'], mdc3: ['TicketDispenserMDC3', 'ticketDispenserMDC3'] },
    'cashbox': { mdc2: ['CashboxMDC2', 'cashboxMDC2'], mdc3: ['CashboxMDC3', 'cashboxMDC3'] },
    'parcel': { mdc2: ['ParcelMDC2', 'parcelMDC2'], mdc3: ['ParcelMDC3', 'parcelMDC3'] },
    'potluck': { mdc2: ['PotluckMDC2', 'potluckMDC2'], mdc3: ['PotluckMDC3', 'potluckMDC3'] },
    'departures': { mdc2: ['DeparturesMDC2', 'departuresMDC2'], mdc3: ['DeparturesMDC3', 'departuresMDC3'] },
    'scoreboard': { mdc2: ['ScoreboardMDC2', 'scoreboardMDC2'], mdc3: ['ScoreboardMDC3', 'scoreboardMDC3'] },
    'reorder': { mdc2: ['ReorderMDC2', 'reorderMDC2'], mdc3: ['ReorderMDC3', 'reorderMDC3'], html: ['ReorderHTML', 'reorderHTML'] },
    'restaurant-menu': ['RestaurantMenu', 'restaurantMenu'],
    'espresso-bar': { mdc3: ['EspressoBarMDC3', 'espressoBarMDC3'], mdc2: ['EspressoBarMDC2', 'espressoBarMDC2'] },
    'order-dashboard': { mdc3: ['OrderDashboardMDC3', 'orderDashboardMDC3'] },
    'product-review': { shoelace: ['ProductReviewShoelace', 'productReviewShoelace'] },
    'meeting-booker': { fluent: ['MeetingBookerFluent', 'meetingBookerFluent'] },
    'loan-calculator': { bootstrap: ['LoanCalculatorBootstrap', 'loanCalculatorBootstrap'] },
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

// Every demo as { set, name, family, variant, mod, fn, dir, bundle, page }.
// `dir`, `bundle` and `page` are repo-relative and also the paths served
// under the deployed /bambik/demo/ tree. An entry naming its design systems
// is one directory and one page: each variant bundles to bundle-<variant>.js
// beside the page, which loads the one its query names (`inbox/?mdc3`). A
// plain [mod, fn] entry (no design system, or a law bench) is a directory of
// its own with bundle.js.

export const all = Object.entries(sets).flatMap(([set, demos]) =>
  Object.entries(demos).flatMap(([family, entry]) => {
    const dir = `demo/${set}/${family}`
    if (Array.isArray(entry)) {
      const [mod, fn] = entry
      return [{ set, name: family, family, variant: null, mod, fn, dir, bundle: `${dir}/bundle.js`, page: `${dir}/` }]
    }
    return Object.entries(entry).map(([variant, [mod, fn]]) => ({
      set, name: `${family}-${variant}`, family, variant, mod, fn, dir,
      bundle: `${dir}/bundle-${variant}.js`, page: `${dir}/?${variant}`,
    }))
  }))

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
