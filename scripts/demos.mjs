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
    // MD3 (PUI.MDC3) siblings of the MDC2 demos — same app, import switched
    'counter-md3': ['CounterMD3', 'counterMD3'],
    'temperature-converter-md3': ['TemperatureConverterMD3', 'temperatureConverterMD3'],
    'flight-booker-md3': ['FlightBookerMD3', 'flightBookerMD3'],
    'timer-md3': ['TimerMD3', 'timerMD3'],
    'crud-md3': ['CrudMD3', 'crudMD3'],
    'circle-drawer-md3': ['CircleDrawerMD3', 'circleDrawerMD3'],
    'cells-md3': ['CellsMD3', 'cellsMD3'],
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
    // MD3 (PUI.MDC3) siblings of the MDC2 demos — same app, import switched
    'order-form-md3': ['OrderFormMD3', 'orderFormMD3'],
    'todomvc-md3': ['TodoMvcMD3', 'todoMvcMD3'],
    'tip-calculator-md3': ['TipCalculatorMD3', 'tipCalculatorMD3'],
    'quiz-md3': ['QuizMD3', 'quizMD3'],
    'tic-tac-toe-md3': ['TicTacToeMD3', 'ticTacToeMD3'],
    'markdown-previewer-md3': ['MarkdownPreviewerMD3', 'markdownPreviewerMD3'],
    'calculator-md3': ['CalculatorMD3', 'calculatorMD3'],
    'stopwatch-md3': ['StopwatchMD3', 'stopwatchMD3'],
    'shopping-cart-md3': ['ShoppingCartMD3', 'shoppingCartMD3'],
    'password-generator-md3': ['PasswordGeneratorMD3', 'passwordGeneratorMD3'],
    'color-mixer-md3': ['ColorMixerMD3', 'colorMixerMD3'],
    'signup-form-md3': ['SignupFormMD3', 'signupFormMD3'],
    'photo-gallery-md3': ['PhotoGalleryMD3', 'photoGalleryMD3'],
    'inbox-md3': ['InboxMD3', 'inboxMD3'],
    'movie-browser-md3': ['MovieBrowserMD3', 'movieBrowserMD3'],
    'weather-md3': ['WeatherMD3', 'weatherMD3'],
    'auction-md3': ['AuctionMD3', 'auctionMD3'],
    'checkout-md3': ['CheckoutMD3', 'checkoutMD3'],
    'payment-md3': ['PaymentMD3', 'paymentMD3'],
    'ticket-dispenser-md3': ['TicketDispenserMD3', 'ticketDispenserMD3'],
    'potluck-md3': ['PotluckMD3', 'potluckMD3'],
    'departures-md3': ['DeparturesMD3', 'departuresMD3'],
    'scoreboard-md3': ['ScoreboardMD3', 'scoreboardMD3'],
    'reorder-md3': ['ReorderMD3', 'reorderMD3'],
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
