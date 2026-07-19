// Bundle every nGUIs demo: each page is module <Name> entered at its own
// named function, so the entry is synthesized here instead of
// spago bundle-app's fixed Main.main.
import { build } from 'esbuild'

const demos = {
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
  'reorder': ['Reorder', 'reorder'],
  'restaurant-menu': ['RestaurantMenu', 'restaurantMenu'],
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
    outfile: `demo/nguis/${dir}/bundle.js`,
  })
  console.log(`bundled ${dir} (${mod}.${fn})`)
}
