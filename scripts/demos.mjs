// The demo registry: every demo, its directory under demo/, and the named
// module + entry function it is entered at. Single source of truth shared by
// the bundle scripts and the dev server — no demo is a `Main` module, so
// `spago bundle-app`'s fixed Main.main is never usable and each entry is
// synthesized from this table instead.

export const sets = {
  '7guis': {
    'counter': ['Counter', 'counter'],
    'temperature-converter': ['TemperatureConverter', 'temperatureConverter'],
    'flight-booker': ['FlightBooker', 'flightBooker'],
    'timer': ['Timer', 'timer'],
    'crud': ['Crud', 'crud'],
    'circle-drawer': ['CircleDrawer', 'circleDrawer'],
    'cells': ['Cells', 'cells'],
    // MDC3 (PUI.MDC3) siblings of the MDC2 demos — same app, import switched
    'counter-mdc3': ['CounterMDC3', 'counterMDC3'],
    'temperature-converter-mdc3': ['TemperatureConverterMDC3', 'temperatureConverterMDC3'],
    'flight-booker-mdc3': ['FlightBookerMDC3', 'flightBookerMDC3'],
    'timer-mdc3': ['TimerMDC3', 'timerMDC3'],
    'crud-mdc3': ['CrudMDC3', 'crudMDC3'],
    'circle-drawer-mdc3': ['CircleDrawerMDC3', 'circleDrawerMDC3'],
    'cells-mdc3': ['CellsMDC3', 'cellsMDC3'],
  },
  'nguis': {
    'order-form': ['OrderForm', 'orderForm'],
    'todomvc': ['TodoMvc', 'todoMvc'],
    'tip-calculator': ['TipCalculator', 'tipCalculator'],
    'quiz': ['Quiz', 'quiz'],
    'tic-tac-toe': ['TicTacToe', 'ticTacToe'],
    'markdown-previewer': ['MarkdownPreviewer', 'markdownPreviewer'],
    'helloworld': ['HelloWorld', 'helloWorld'],
    'calculator': ['Calculator', 'calculator'],
    'stopwatch': ['Stopwatch', 'stopwatch'],
    'shopping-cart': ['ShoppingCart', 'shoppingCart'],
    'password-generator': ['PasswordGenerator', 'passwordGenerator'],
    'color-mixer': ['ColorMixer', 'colorMixer'],
    'signup-form': ['SignupForm', 'signupForm'],
    'photo-gallery': ['PhotoGallery', 'photoGallery'],
    'inbox': ['Inbox', 'inbox'],
    'movie-browser': ['MovieBrowser', 'movieBrowser'],
    'weather': ['Weather', 'weather'],
    'auction': ['Auction', 'auction'],
    'checkout': ['Checkout', 'checkout'],
    'payment': ['Payment', 'payment'],
    'ticket-dispenser': ['TicketDispenser', 'ticketDispenser'],
    'potluck': ['Potluck', 'potluck'],
    'departures': ['Departures', 'departures'],
    'scoreboard': ['Scoreboard', 'scoreboard'],
    'reorder': ['Reorder', 'reorder'],
    'restaurant-menu': ['RestaurantMenu', 'restaurantMenu'],
    'espresso-bar': ['EspressoBar', 'espressoBar'],
    // MDC3 (PUI.MDC3) siblings of the MDC2 demos — same app, import switched
    'order-form-mdc3': ['OrderFormMDC3', 'orderFormMDC3'],
    'todomvc-mdc3': ['TodoMvcMDC3', 'todoMvcMDC3'],
    'tip-calculator-mdc3': ['TipCalculatorMDC3', 'tipCalculatorMDC3'],
    'quiz-mdc3': ['QuizMDC3', 'quizMDC3'],
    'tic-tac-toe-mdc3': ['TicTacToeMDC3', 'ticTacToeMDC3'],
    'markdown-previewer-mdc3': ['MarkdownPreviewerMDC3', 'markdownPreviewerMDC3'],
    'calculator-mdc3': ['CalculatorMDC3', 'calculatorMDC3'],
    'stopwatch-mdc3': ['StopwatchMDC3', 'stopwatchMDC3'],
    'shopping-cart-mdc3': ['ShoppingCartMDC3', 'shoppingCartMDC3'],
    'password-generator-mdc3': ['PasswordGeneratorMDC3', 'passwordGeneratorMDC3'],
    'color-mixer-mdc3': ['ColorMixerMDC3', 'colorMixerMDC3'],
    'signup-form-mdc3': ['SignupFormMDC3', 'signupFormMDC3'],
    'photo-gallery-mdc3': ['PhotoGalleryMDC3', 'photoGalleryMDC3'],
    'inbox-mdc3': ['InboxMDC3', 'inboxMDC3'],
    'movie-browser-mdc3': ['MovieBrowserMDC3', 'movieBrowserMDC3'],
    'weather-mdc3': ['WeatherMDC3', 'weatherMDC3'],
    'auction-mdc3': ['AuctionMDC3', 'auctionMDC3'],
    'checkout-mdc3': ['CheckoutMDC3', 'checkoutMDC3'],
    'payment-mdc3': ['PaymentMDC3', 'paymentMDC3'],
    'ticket-dispenser-mdc3': ['TicketDispenserMDC3', 'ticketDispenserMDC3'],
    'potluck-mdc3': ['PotluckMDC3', 'potluckMDC3'],
    'departures-mdc3': ['DeparturesMDC3', 'departuresMDC3'],
    'scoreboard-mdc3': ['ScoreboardMDC3', 'scoreboardMDC3'],
    'reorder-mdc3': ['ReorderMDC3', 'reorderMDC3'],
  },
}

// Every demo as { set, name, dir, mod, fn } — dir is repo-relative, and also
// the path the page is served at under the deployed /bambik/demo/ tree.
export const all = Object.entries(sets).flatMap(([set, demos]) =>
  Object.entries(demos).map(([name, [mod, fn]]) => ({
    set, name, mod, fn, dir: `demo/${set}/${name}`,
  })))

export const inSet = set => all.filter(d => d.set === set)

export const entryFor = ({ mod, fn }) =>
  `import { ${fn} } from './output/${mod}/index.js'; ${fn}();`
