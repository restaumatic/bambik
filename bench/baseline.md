# bambik on Diner — 2026-11-20, d807c07b+dirty

11th Gen Intel(R) Core(TM) i7-11850H @ 2.50GHz, 16 threads, 63 GB; Chrome/158.0.8127.36; node v25.6.0; 1045 s.

### View model in node: one feed's reads per station (ms)

| station    | 1×    | 4×     | 16×     | 64×     | growth |
| ---------- | ----- | ------ | ------- | ------- | ------ |
| Magazyn    | 0.44  | 1.823  | 8.175   | 33.576  | 1.05   |
| Sala       | 0.437 | 1.869  | 8.358   | 38.017  | 1.07   |
| Kuchnia    | 0.09  | 0.315  | 1.716   | 9.83    | 1.14   |
| Wydawka    | 0.03  | 0.114  | 0.498   | 2.317   | 1.04   |
| Raport     | 0.149 | 0.666  | 3.168   | 15.759  | 1.12   |
| Statystyki | 5.622 | 28.896 | 151.831 | 776.941 | 1.19   |

The costliest reads at the largest scale:

| read                         | 1×    | 4×    | 16×    | 64×     | growth |
| ---------------------------- | ----- | ----- | ------ | ------- | ------ |
| Statystyki: pageLine         | 0.807 | 4.037 | 21.416 | 122.126 | 1.21   |
| Statystyki: searchState      | 0.833 | 4.388 | 26.084 | 113.657 | 1.19   |
| Statystyki: soldQuantityLine | 0.765 | 4.147 | 21.047 | 112.364 | 1.2    |
| Statystyki: salesPage        | 0.896 | 4.366 | 20.681 | 109.396 | 1.15   |
| Statystyki: soldAmountLine   | 0.775 | 4.019 | 20.948 | 106.398 | 1.18   |
| Statystyki: isFirstPage      | 0.758 | 3.934 | 20.599 | 105.961 | 1.19   |
| Statystyki: isLastPage       | 0.772 | 3.94  | 20.785 | 105.889 | 1.18   |
| Sala: menuPage               | 0.172 | 0.808 | 4.006  | 19.058  | 1.13   |
| Magazyn: stockPage           | 0.212 | 0.879 | 4.493  | 18.627  | 1.09   |
| Raport: paidPage             | 0.06  | 0.304 | 1.698  | 9.653   | 1.22   |

Generating the day (`dayAt`): 1× 1.33 ms, 4× 5.79 ms, 16× 33.53 ms, 64× 72.5 ms.

### Responsiveness: what one doing costs the browser (ms)

Script, style and layout together; the split is the median at full speed. The budget is on the 95th percentile at a quarter of the speed: 100 ms for something done, 16 ms for a second passing, 1 s for the start.

| done                                     | n  | p50   | p95   | script / style / layout | p50 at ¼ | p95 at ¼ | budget   |
| ---------------------------------------- | -- | ----- | ----- | ----------------------- | -------- | -------- | -------- |
| stock: a portion more                    | 30 | 3.8   | 5.34  | 3.01 / 0.44 / 0.34      | 13.54    | 21.41    | within   |
| stock: a portion fewer                   | 30 | 3.63  | 6.06  | 2.95 / 0.42 / 0.32      | 13.65    | 19.78    | within   |
| stock: a key in a portion count          | 30 | 2.39  | 2.96  | 2.16 / 0 / 0.16         | 10.49    | 13.73    | within   |
| stock: a key in a price                  | 30 | 2.41  | 3.16  | 2.18 / 0 / 0.17         | 9.29     | 11.66    | within   |
| stock: a page turned                     | 30 | 20.31 | 22.03 | 12.64 / 4.92 / 2.61     | 82.63    | 97.52    | within   |
| stock: a group picked                    | 30 | 21.61 | 25.5  | 12.31 / 6.49 / 2.71     | 92.64    | 107.85   | over 100 |
| stock: a key in the search               | 32 | 4.19  | 19.85 | 3.52 / 0.28 / 0.25      | 14.61    | 79.07    | within   |
| hall: a dish added                       | 30 | 3.99  | 7.44  | 3.52 / 0.19 / 0.36      | 14.92    | 29.44    | within   |
| hall: a portion more on a line           | 30 | 5.31  | 8.77  | 4.36 / 0.52 / 0.44      | 19.64    | 25.08    | within   |
| hall: a portion fewer on a line          | 30 | 5.68  | 8.29  | 4.47 / 0.75 / 0.49      | 21.64    | 28.74    | within   |
| hall: a table picked                     | 30 | 5.69  | 7.38  | 4.22 / 1.09 / 0.84      | 22.48    | 27.7     | within   |
| hall: a page turned                      | 30 | 7.76  | 9.93  | 5.82 / 0.97 / 0.83      | 29.02    | 43.44    | within   |
| hall: a group picked                     | 30 | 8.27  | 13.42 | 5.73 / 1.9 / 0.77       | 35.1     | 74.66    | within   |
| hall: a key in the search                | 32 | 5.64  | 7.96  | 4.37 / 0.37 / 0.71      | 23.5     | 29.71    | within   |
| kitchen: a page turned                   | 30 | 14.12 | 18.17 | 9.05 / 3.03 / 1.7       | 55.61    | 70.48    | within   |
| kitchen: a line plated                   | 7  | 3.95  | 4.37  | 3.01 / 0.75 / 0.29      | 13.05    | 18.1     | within   |
| kitchen: an order passed on, asked       | 12 | 4.2   | 5.55  | 0.34 / 3.48 / 0.35      | 17.88    | 28.41    | within   |
| kitchen: an order passed on, confirmed   | 12 | 18.21 | 20.71 | 4.3 / 10.83 / 2.87      | 57.14    | 71.84    | within   |
| pass: a payment, asked                   | 10 | 2.72  | 5.08  | 0.44 / 1.86 / 0.44      | 9.89     | 19.43    | within   |
| pass: a payment, confirmed               | 10 | 11.33 | 12.71 | 1.7 / 7.97 / 1.76       | 28.77    | 35.5     | within   |
| report: a page turned                    | 30 | 3.99  | 5.98  | 2.61 / 0.81 / 0.64      | 13.74    | 18.8     | within   |
| statistics: a page turned                | 30 | 11.87 | 15.48 | 10.57 / 0.76 / 0.6      | 42.49    | 57.03    | within   |
| statistics: a group picked               | 30 | 15.51 | 18.55 | 13 / 1.91 / 0.63        | 58.3     | 67.07    | within   |
| statistics: a key in the search          | 32 | 14.12 | 16.34 | 13.42 / 0.17 / 0.56     | 53.07    | 69.64    | within   |
| app: a second passes at Magazyn          | 6  | 2.25  | 3.13  | 2.17 / 0.14 / 0         | 11.94    | 13.47    | within   |
| app: a second passes at Sala             | 6  | 2.86  | 3.24  | 2.7 / 0.19 / 0          | 11.4     | 15.96    | within   |
| app: a second passes at Kuchnia          | 6  | 1.57  | 1.75  | 1.38 / 0.22 / 0         | 5.28     | 6.41     | within   |
| app: a second passes at Wydawka          | 6  | 0.92  | 1.56  | 0.81 / 0.15 / 0         | 3.06     | 5.07     | within   |
| app: a second passes at Raport           | 6  | 1.6   | 1.82  | 1.46 / 0.27 / 0         | 3.63     | 5.28     | within   |
| app: a second passes at Statystyki       | 6  | 7.55  | 9.12  | 7.44 / 0.16 / 0         | 32.63    | 36.39    | over 16  |
| app: switched to Magazyn                 | 5  | 15.27 | 17.5  | 5.1 / 7.38 / 2.72       | 66.32    | 67.88    | within   |
| app: switched to Sala                    | 5  | 14.78 | 15.56 | 7.33 / 5.28 / 1.78      | 60.07    | 77.82    | within   |
| app: switched to Kuchnia                 | 5  | 9.58  | 11.84 | 2.99 / 5.13 / 1.66      | 42.65    | 45.74    | within   |
| app: switched to Wydawka                 | 5  | 5.8   | 6.31  | 1.98 / 2.91 / 0.91      | 22.68    | 25.38    | within   |
| app: switched to Raport                  | 5  | 4.63  | 5.66  | 1.93 / 2 / 0.68         | 22.18    | 24.27    | within   |
| app: switched to Statystyki              | 5  | 20.84 | 23.96 | 14.27 / 4.08 / 1.08     | 87.8     | 106.04   | over 100 |
| app: started, until it waits for its day | 6  | 45.73 | 49.66 | 40.58 / 2.16 / 3.53     | 180.74   | 201.56   | within   |
| app: started, the day drawn              | 6  | 53.07 | 57.49 | 38.93 / 3.88 / 10.13    | 221.81   | 231.34   | within   |

Where the time of a station's doings goes (CPU profile, self time by layer):

| station    | ms profiled | view model | carrier | design system | browser | gc |
| ---------- | ----------- | ---------- | ------- | ------------- | ------- | -- |
| Stock      | 1435        | 5%         | 20%     | 35%           | 36%     | 4% |
| Hall       | 864         | 15%        | 33%     | 7%            | 40%     | 5% |
| Kitchen    | 811         | 2%         | 17%     | 22%           | 56%     | 3% |
| Pass       | 297         | 2%         | 4%      | 17%           | 78%     | 1% |
| Report     | 96          | 9%         | 23%     | 7%            | 56%     | 5% |
| Statistics | 702         | 58%        | 10%     | 6%            | 21%     | 5% |

Stock, the functions that took most: (program) 511.96 ms; scrollToTab (node_modules/@material/web/tabs/internal/tabs.js) 75.87 ms; (garbage collector) 55.16 ms; showStringImpl (Data.Show) 34.53 ms; insertBefore 29.76 ms.
Hall, the functions that took most: (program) 327.85 ms; (garbage collector) 41.6 ms; (anonymous) (Effect) 31.73 ms; setAttribute 23.44 ms; __do3 (PUI) 21.19 ms.
Kitchen, the functions that took most: (program) 443.26 ms; animate 49.66 ms; showModal 32.88 ms; (garbage collector) 28 ms; appendChild 14.41 ms.
Pass, the functions that took most: (program) 223.6 ms; animate 17.36 ms; showModal 15.83 ms; (anonymous) 4.11 ms; animateDialog (node_modules/@material/web/dialog/internal/dialog.js) 2.33 ms.
Report, the functions that took most: (program) 53.52 ms; (garbage collector) 4.88 ms; setAttribute 4.05 ms; (anonymous) (Control.Bind) 1.81 ms; (anonymous) (Control.Monad) 1.69 ms.
Statistics, the functions that took most: (program) 144.56 ms; go (Data.Map.Internal) 100.94 ms; mergeFromTo (Data.Array) 73.91 ms; (anonymous) (Data.Map.Internal) 48.69 ms; (anonymous) (Data.Ord) 48.52 ms.

No exception and no console warning.

### Large data: a station alone on days of growing load (ms)

The standard day is 1×; 64× is 19456 dishes, 2688 tables and 16128 orders. Growth is the exponent k of cost ≈ c·scale^k: 0 is flat, 1 linear, above 1 a finding.

| station    | measured        | 1×    | 4×     | 16×    | 64×     | growth |
| ---------- | --------------- | ----- | ------ | ------ | ------- | ------ |
| Stock      | drawn           | 82.01 | 102.47 | 145.41 | 285.98  | 0.3    |
| Stock      | a second passes | 2.69  | 5.41   | 14.34  | 53.26   | 0.72   |
| Stock      | a portion more  | 4.14  | 8.97   | 22.76  | 89.86   | 0.73   |
| Stock      | DOM nodes       | 6184  | 6192   | 6192   | 6192    | 0      |
| Stock      | heap (MB)       | 6.3   | 5.9    | 10.3   | 23.4    | 0.32   |
| Hall       | drawn           | 82.95 | 135.73 | 286.01 | 814.17  | 0.55   |
| Hall       | a second passes | 3.6   | 7.31   | 24.16  | 91.78   | 0.79   |
| Hall       | a dish added    | 4.58  | 9.29   | 27.2   | 104.21  | 0.75   |
| Hall       | DOM nodes       | 2457  | 5121   | 15777  | 54023   | 0.75   |
| Hall       | heap (MB)       | 7.1   | 11.6   | 18.7   | 51.2    | 0.46   |
| Kitchen    | drawn           | 69.95 | 73.9   | 99.89  | 167.4   | 0.21   |
| Kitchen    | a second passes | 1.75  | 2.3    | 4.24   | 10      | 0.42   |
| Kitchen    | a page turned   | 16.29 | 15.64  | 17.36  | 24.09   | 0.09   |
| Kitchen    | DOM nodes       | 3286  | 3143   | 3325   | 2896    | -0.02  |
| Kitchen    | heap (MB)       | 4.8   | 7.4    | 9.6    | 17.5    | 0.3    |
| Pass       | drawn           | 50.52 | 58.39  | 77.47  | 144.08  | 0.25   |
| Pass       | a second passes | 0.82  | 1.44   | 1.91   | 4.46    | 0.39   |
| Pass       | a payment taken | 14.81 | 17.09  | 18.01  | 22.56   | 0.09   |
| Pass       | DOM nodes       | 922   | 924    | 924    | 924     | 0      |
| Pass       | heap (MB)       | 4.5   | 4.6    | 7.7    | 13.9    | 0.28   |
| Report     | drawn           | 44.3  | 53.55  | 88.06  | 162.74  | 0.32   |
| Report     | a second passes | 1.23  | 1.87   | 3.61   | 16.89   | 0.61   |
| Report     | a page turned   | 4.31  | 4.77   | 6.55   | 20.72   | 0.36   |
| Report     | DOM nodes       | 301   | 301    | 301    | 301     | 0      |
| Report     | heap (MB)       | 5     | 6.2    | 6.5    | 41.3    | 0.46   |
| Statistics | drawn           | 78.94 | 152.18 | 404.61 | 1642.82 | 0.73   |
| Statistics | a second passes | 8.35  | 32.41  | 131.98 | 712.44  | 1.06   |
| Statistics | a page turned   | 12.34 | 35.41  | 160.52 | 815.74  | 1.02   |
| Statistics | DOM nodes       | 924   | 924    | 924    | 905     | 0      |
| Statistics | heap (MB)       | 4.7   | 8.5    | 21.2   | 42.2    | 0.54   |

Where drawing the day at 64× goes (CPU profile, self time by layer):

| station    | ms profiled | view model | carrier | design system | browser | gc |
| ---------- | ----------- | ---------- | ------- | ------------- | ------- | -- |
| Stock      | 356         | 50%        | 20%     | 16%           | 8%      | 6% |
| Hall       | 1072        | 20%        | 41%     | 12%           | 18%     | 9% |
| Kitchen    | 222         | 54%        | 10%     | 11%           | 16%     | 9% |
| Pass       | 183         | 55%        | 9%      | 11%           | 16%     | 9% |
| Report     | 222         | 65%        | 6%      | 7%            | 12%     | 9% |
| Statistics | 1801        | 88%        | 1%      | 2%            | 2%      | 8% |

With pagination off, everything listed is on screen. A row's cost is the difference to the paged station over the difference in rows:

| station    | scale | listed | drawn   | per row | a second passes | per row | DOM nodes |
| ---------- | ----- | ------ | ------- | ------- | --------------- | ------- | --------- |
| Stock      | 1×    | 304    | 703.77  | 2.115   | 21.17           | 0.063   | 144136    |
| Stock      | 4×    | 1216   | 2307.39 | 1.828   | 94.78           | 0.074   | 641133    |
| Hall       | 1×    | 346    | 168.22  | 0.305   | 12.56           | 0.032   | 6655      |
| Hall       | 4×    | 1384   | 449.21  | 0.263   | 48.02           | 0.034   | 22999     |
| Kitchen    | 1×    | 20     | 92.26   | 2.231   | 2.73            | 0.098   | 6085      |
| Kitchen    | 4×    | 80     | 183.52  | 1.566   | 7.05            | 0.068   | 22345     |
| Pass       | 1×    | 10     | 50.48   | —       | 0.71            | —       | 922       |
| Pass       | 4×    | 40     | 71.98   | 0.453   | 2.34            | 0.03    | 2422      |
| Report     | 1×    | 220    | 92.72   | 0.232   | 6.21            | 0.024   | 2598      |
| Report     | 4×    | 877    | 226.83  | 0.2     | 22.1            | 0.023   | 9825      |
| Statistics | 1×    | 263    | 142.19  | 0.251   | 18.4            | 0.04    | 3442      |
| Statistics | 4×    | 1040   | 426.47  | 0.267   | 82.33           | 0.049   | 11212     |

### Correctness under stress: a random walk against the view model

3000 steps of real input (seed 20261120, 167 s): after every one the station on screen read exactly what the view model says of the reference model. The day ended with 310 orders, 288 of them paid.

Done along the way: open ×1177, next page ×326, previous page ×239, add ×186, search ×162, order ×159, serve table ×136, plate ×125, stock + ×76, stock − ×64, price of ×61, group ×60, portions of ×53, line + ×48, zero ×45, send ×41, remove ×27, line − ×15.

No exception and no console warning.

### Races: the application driven faster than a person can

- holds: twelve clicks on a dish with no wait between them put twelve portions on the order
- holds: three digits typed while 3 seconds pass stay as typed, the caret after them
- holds: and are the stock when the stockroom is come back to
- holds: a dish clicked while the order was on its way went with it or stayed in stock, never both or neither
- holds: the last page losing its only row falls back to the page before
- holds: a payment confirmed twice at once is taken once

No exception and no console warning.

### Reliability: left running

5300 random clicks over 240 s under the clock, sampled every 25 after a forced collection. The slope is over the second half of the run:

|           | first sample | last sample | slope per minute |
| --------- | ------------ | ----------- | ---------------- |
| heap (MB) | 6.6          | 284.3       | 60.87            |
| DOM nodes | 8674         | 1418490     | 297197.38        |
| listeners | 946          | 150274      | 31705.59         |

What one doing leaves behind for good (twenty of them between two forced collections, a station alone):

| done                      | DOM nodes | listeners | heap (KB) |
| ------------------------- | --------- | --------- | --------- |
| stockroom: a page turned  | 4619.9    | 490       | 715       |
| hall: a page turned       | 359.9     | 24        | 279.9     |
| hall: a group picked      | 330       | 22        | 257.1     |
| hall: a table picked      | 0         | 0         | 4.8       |
| kitchen: a page turned    | 2446.4    | 265.5     | 582.3     |
| report: a page turned     | 109.9     | 0         | 67.4      |
| statistics: a page turned | 99.9      | 0         | 59.9      |

900 station switches after a warm-up of twelve: heap 7.2 → 7.8 MB, DOM nodes 11623 → 11623, listeners 1275 → 1275.

The clock, watched for 60 s with the hall open: 0.9969 of its seconds pass in a second (each beat waits a full second after the one before has been handled). Across 7.2 s with the page frozen for 6 of them it counted 1.

No exception and no console warning.

### Application size: synthetic applications of growing width

Each station is a display, an editor, a keyed table of five rows and three buttons with their folds; one model, one knot. Compile time is for the two modules (view and view model), over the 0.2 s the compiler needs with nothing to do. Growth is the exponent against the number of stations.

|                                   | 6 stations | 12 stations | 24 stations | 48 stations | growth |
| --------------------------------- | ---------- | ----------- | ----------- | ----------- | ------ |
| compile (s)                       | 0.3        | 0.7         | 2.2         | 8.7         | 1.62   |
| bundle, minified (KB)             | 498.5      | 519.7       | 592.5       | 856.8       | 0.25   |
| bundle, gzipped (KB)              | 95.3       | 96.9        | 100.3       | 108         | 0.06   |
| drawn (ms)                        | 44.24      | 54.81       | 60.44       | 78.41       | 0.26   |
| DOM nodes                         | 966        | 1356        | 2136        | 3696        | 0.65   |
| a tick fed, one station open (ms) | 0.54       | 0.58        | 0.59        | 0.86        | 0.2    |
| a button clicked (ms)             | 1.23       | 1.17        | 2.31        | 1.36        | 0.14   |

No exception and no console warning.

