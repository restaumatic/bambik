# bambik on Diner — 2026-11-21, 2a800c94+dirty

11th Gen Intel(R) Core(TM) i7-11850H @ 2.50GHz, 16 threads, 63 GB; Chrome/158.0.8127.36; node v25.6.0; 1041 s.

### View model in node: one feed's reads per station (ms)

| station    | 1×    | 4×     | 16×     | 64×     | growth |
| ---------- | ----- | ------ | ------- | ------- | ------ |
| Magazyn    | 0.451 | 1.864  | 7.512   | 36.218  | 1.05   |
| Sala       | 0.429 | 1.928  | 8.052   | 37.959  | 1.07   |
| Kuchnia    | 0.066 | 0.31   | 1.726   | 9.205   | 1.19   |
| Wydawka    | 0.03  | 0.11   | 0.525   | 2.461   | 1.07   |
| Raport     | 0.159 | 0.634  | 3.307   | 16.536  | 1.12   |
| Statystyki | 5.358 | 28.154 | 146.569 | 805.529 | 1.2    |

The costliest reads at the largest scale:

| read                         | 1×    | 4×    | 16×    | 64×     | growth |
| ---------------------------- | ----- | ----- | ------ | ------- | ------ |
| Statystyki: isFirstPage      | 0.747 | 3.86  | 20.719 | 126.86  | 1.23   |
| Statystyki: soldAmountLine   | 0.772 | 4.038 | 20.542 | 120.488 | 1.21   |
| Statystyki: soldQuantityLine | 0.757 | 4     | 20.882 | 115.78  | 1.21   |
| Statystyki: searchState      | 0.814 | 3.876 | 24.186 | 111.752 | 1.2    |
| Statystyki: pageLine         | 0.745 | 3.884 | 19.375 | 111.155 | 1.2    |
| Statystyki: salesPage        | 0.753 | 3.877 | 19.196 | 110.365 | 1.19   |
| Statystyki: isLastPage       | 0.753 | 4.545 | 21.353 | 107.972 | 1.19   |
| Magazyn: stockPage           | 0.196 | 0.879 | 3.82   | 20.67   | 1.11   |
| Sala: menuPage               | 0.161 | 0.834 | 3.654  | 18.96   | 1.14   |
| Raport: paidPage             | 0.072 | 0.299 | 1.774  | 9.934   | 1.19   |

Generating the day (`dayAt`): 1× 3.1 ms, 4× 6.58 ms, 16× 34.63 ms, 64× 85.62 ms.

### Responsiveness: what one doing costs the browser (ms)

Script, style and layout together; the split is the median at full speed. The budget is on the 95th percentile at a quarter of the speed: 100 ms for something done, 16 ms for a second passing, 1 s for the start.

| done                                     | n  | p50   | p95   | script / style / layout | p50 at ¼ | p95 at ¼ | budget   |
| ---------------------------------------- | -- | ----- | ----- | ----------------------- | -------- | -------- | -------- |
| stock: a portion more                    | 30 | 3.29  | 4.51  | 2.68 / 0.33 / 0.3       | 14.28    | 19.04    | within   |
| stock: a portion fewer                   | 30 | 3.62  | 5.26  | 2.91 / 0.37 / 0.3       | 13.6     | 18.94    | within   |
| stock: a key in a portion count          | 30 | 2.45  | 2.98  | 2.27 / 0 / 0.15         | 9.93     | 11.52    | within   |
| stock: a key in a price                  | 30 | 2.33  | 3.28  | 2.11 / 0 / 0.16         | 9.08     | 11.34    | within   |
| stock: a page turned                     | 30 | 19.89 | 30.81 | 11.91 / 4.7 / 2.67      | 81.78    | 105.09   | over 100 |
| stock: a group picked                    | 30 | 22.33 | 31.34 | 12.25 / 6.56 / 2.72     | 87.54    | 105.45   | over 100 |
| stock: a key in the search               | 32 | 3.78  | 17.72 | 3.54 / 0.21 / 0.23      | 14.45    | 71.49    | within   |
| hall: a dish added                       | 30 | 3.91  | 6.96  | 3.4 / 0.18 / 0.34       | 15.91    | 27.82    | within   |
| hall: a portion more on a line           | 30 | 5.03  | 7.2   | 4.1 / 0.53 / 0.4        | 20.32    | 26.61    | within   |
| hall: a portion fewer on a line          | 30 | 5.19  | 5.67  | 4.08 / 0.66 / 0.46      | 21.73    | 32.04    | within   |
| hall: a table picked                     | 30 | 6.27  | 9.26  | 4.52 / 1.13 / 0.84      | 22.48    | 34.85    | within   |
| hall: a page turned                      | 30 | 7.46  | 10    | 5.62 / 1 / 0.82         | 29.75    | 36.49    | within   |
| hall: a group picked                     | 30 | 8.44  | 12.39 | 5.65 / 1.88 / 0.77      | 36.34    | 49.59    | within   |
| hall: a key in the search                | 32 | 5.33  | 7.7   | 4.33 / 0.36 / 0.67      | 22.53    | 28.01    | within   |
| kitchen: a page turned                   | 30 | 13.44 | 16.64 | 8.74 / 2.94 / 1.67      | 54.75    | 66.27    | within   |
| kitchen: a line plated                   | 7  | 3.61  | 4.01  | 2.62 / 0.69 / 0.28      | 14.39    | 18.74    | within   |
| kitchen: an order passed on, asked       | 12 | 4.36  | 6.34  | 0.38 / 3.63 / 0.36      | 17.34    | 23.85    | within   |
| kitchen: an order passed on, confirmed   | 12 | 18.62 | 21.48 | 3.84 / 12.29 / 3.02     | 60.01    | 67.59    | within   |
| pass: a payment, asked                   | 10 | 2.87  | 4.66  | 0.44 / 2.07 / 0.43      | 10.96    | 21.65    | within   |
| pass: a payment, confirmed               | 10 | 11.03 | 12.95 | 1.88 / 7.51 / 1.64      | 33.53    | 41.04    | within   |
| report: a page turned                    | 30 | 3.6   | 5.82  | 2.19 / 0.78 / 0.63      | 12.28    | 22.65    | within   |
| statistics: a page turned                | 30 | 10.95 | 13.5  | 9.75 / 0.82 / 0.58      | 43.42    | 52.34    | within   |
| statistics: a group picked               | 30 | 14.41 | 18.43 | 12.09 / 1.81 / 0.58     | 58.27    | 64.93    | within   |
| statistics: a key in the search          | 32 | 13.23 | 17.57 | 12.5 / 0.14 / 0.55      | 51.23    | 66.08    | within   |
| app: a second passes at Magazyn          | 6  | 2.29  | 3.79  | 2.29 / 0.14 / 0         | 10.24    | 13.31    | within   |
| app: a second passes at Sala             | 6  | 3.05  | 3.82  | 2.95 / 0.17 / 0         | 11.66    | 12.83    | within   |
| app: a second passes at Kuchnia          | 6  | 1.65  | 1.99  | 1.41 / 0.22 / 0         | 5.19     | 5.8      | within   |
| app: a second passes at Wydawka          | 6  | 0.94  | 1.83  | 0.79 / 0.23 / 0         | 3.79     | 5.12     | within   |
| app: a second passes at Raport           | 6  | 1.35  | 1.63  | 1.2 / 0.15 / 0          | 5.51     | 6.19     | within   |
| app: a second passes at Statystyki       | 6  | 8.18  | 9.67  | 8.09 / 0.16 / 0         | 32.5     | 35.86    | over 16  |
| app: switched to Magazyn                 | 5  | 14.8  | 17.08 | 4.62 / 7.64 / 2.73      | 64.09    | 66.6     | within   |
| app: switched to Sala                    | 5  | 14.17 | 20.29 | 6.78 / 5.36 / 1.91      | 61.73    | 87.99    | within   |
| app: switched to Kuchnia                 | 5  | 9.73  | 11.42 | 3.26 / 5.05 / 1.61      | 42.21    | 77.11    | within   |
| app: switched to Wydawka                 | 5  | 5.39  | 7.79  | 1.85 / 2.61 / 0.92      | 24.91    | 28.48    | within   |
| app: switched to Raport                  | 5  | 4.56  | 5.64  | 1.91 / 2.06 / 0.67      | 21.12    | 22.81    | within   |
| app: switched to Statystyki              | 5  | 21.03 | 23.65 | 16.44 / 4.02 / 1.01     | 83.54    | 95.78    | within   |
| app: started, until it waits for its day | 6  | 47.46 | 51.55 | 41.12 / 2.13 / 3.52     | 179.91   | 187.71   | within   |
| app: started, the day drawn              | 6  | 51.87 | 61.11 | 39.79 / 3.72 / 9.94     | 202.1    | 356.92   | within   |

Where the time of a station's doings goes (CPU profile, self time by layer):

| station    | ms profiled | view model | carrier | design system | browser | gc |
| ---------- | ----------- | ---------- | ------- | ------------- | ------- | -- |
| Stock      | 1463        | 5%         | 20%     | 33%           | 37%     | 4% |
| Hall       | 882         | 16%        | 33%     | 7%            | 39%     | 5% |
| Kitchen    | 812         | 2%         | 17%     | 23%           | 55%     | 4% |
| Pass       | 293         | 1%         | 4%      | 18%           | 76%     | 1% |
| Report     | 105         | 6%         | 23%     | 7%            | 59%     | 5% |
| Statistics | 711         | 58%        | 9%      | 6%            | 21%     | 5% |

Stock, the functions that took most: (program) 533.63 ms; scrollToTab (node_modules/@material/web/tabs/internal/tabs.js) 79.14 ms; (garbage collector) 60.89 ms; showStringImpl (Data.Show) 32.09 ms; setAttribute 26.17 ms.
Hall, the functions that took most: (program) 331.25 ms; (garbage collector) 39.87 ms; (anonymous) (Effect) 29.38 ms; __do3 (PUI) 23.92 ms; setAttribute 23.18 ms.
Kitchen, the functions that took most: (program) 435.1 ms; animate 50.7 ms; showModal 34.07 ms; (garbage collector) 29.39 ms; (anonymous) (Effect) 14.14 ms.
Pass, the functions that took most: (program) 217.41 ms; animate 18.53 ms; showModal 17.02 ms; appendChild 2.57 ms; (anonymous) 2.54 ms.
Report, the functions that took most: (program) 60.48 ms; (garbage collector) 5.26 ms; setAttribute 4.3 ms; (anonymous) (Effect) 2.04 ms; (anonymous) (Control.Bind) 2.04 ms.
Statistics, the functions that took most: (program) 143.78 ms; go (Data.Map.Internal) 113.06 ms; mergeFromTo (Data.Array) 73.48 ms; (anonymous) (Data.Ord) 48.52 ms; (garbage collector) 38.41 ms.

No exception and no console warning.

### Large data: a station alone on days of growing load (ms)

The standard day is 1×; 64× is 19456 dishes, 2688 tables and 16128 orders. Growth is the exponent k of cost ≈ c·scale^k: 0 is flat, 1 linear, above 1 a finding.

| station    | measured        | 1×    | 4×     | 16×    | 64×     | growth |
| ---------- | --------------- | ----- | ------ | ------ | ------- | ------ |
| Stock      | drawn           | 84.24 | 98.2   | 138.55 | 295.72  | 0.3    |
| Stock      | a second passes | 2.58  | 5.34   | 13.61  | 55.1    | 0.73   |
| Stock      | a portion more  | 3.86  | 9.3    | 24.36  | 88.83   | 0.75   |
| Stock      | DOM nodes       | 6184  | 6192   | 6192   | 6192    | 0      |
| Stock      | heap (MB)       | 6.2   | 5.9    | 7.8    | 19.1    | 0.26   |
| Hall       | drawn           | 87.47 | 132.64 | 278.45 | 800.04  | 0.53   |
| Hall       | a second passes | 3.94  | 7.44   | 23.32  | 89.41   | 0.76   |
| Hall       | a dish added    | 4.63  | 9.04   | 25.9   | 105.04  | 0.75   |
| Hall       | DOM nodes       | 2457  | 5121   | 15777  | 54023   | 0.75   |
| Hall       | heap (MB)       | 7.2   | 11.7   | 18.8   | 51.2    | 0.46   |
| Kitchen    | drawn           | 66.04 | 73.64  | 105.98 | 166.65  | 0.23   |
| Kitchen    | a second passes | 1.71  | 2.35   | 4.2    | 10.65   | 0.44   |
| Kitchen    | a page turned   | 16.55 | 15.93  | 18.22  | 24.84   | 0.1    |
| Kitchen    | DOM nodes       | 3286  | 3143   | 3325   | 2896    | -0.02  |
| Kitchen    | heap (MB)       | 4.8   | 7.5    | 9.7    | 16.8    | 0.29   |
| Pass       | drawn           | 48.51 | 55.61  | 76.7   | 142.95  | 0.26   |
| Pass       | a second passes | 0.96  | 1.2    | 2.24   | 5.45    | 0.42   |
| Pass       | a payment taken | 15.02 | 17.41  | 18.04  | 23.28   | 0.1    |
| Pass       | DOM nodes       | 922   | 924    | 924    | 924     | 0      |
| Pass       | heap (MB)       | 4.6   | 4.6    | 7.5    | 12.6    | 0.25   |
| Report     | drawn           | 59.24 | 55.86  | 88.01  | 159.89  | 0.25   |
| Report     | a second passes | 1.61  | 2.06   | 3.77   | 16.13   | 0.54   |
| Report     | a page turned   | 4.04  | 4.94   | 7.2    | 18.63   | 0.36   |
| Report     | DOM nodes       | 301   | 301    | 301    | 301     | 0      |
| Report     | heap (MB)       | 5     | 6.3    | 8.9    | 41.2    | 0.48   |
| Statistics | drawn           | 76.8  | 152.16 | 402.52 | 1626.56 | 0.73   |
| Statistics | a second passes | 8.56  | 30.3   | 134.57 | 732.84  | 1.07   |
| Statistics | a page turned   | 12.28 | 36.85  | 158.57 | 814.11  | 1.01   |
| Statistics | DOM nodes       | 924   | 924    | 924    | 905     | 0      |
| Statistics | heap (MB)       | 4.8   | 8      | 30.4   | 42.3    | 0.57   |

Where drawing the day at 64× goes (CPU profile, self time by layer):

| station    | ms profiled | view model | carrier | design system | browser | gc |
| ---------- | ----------- | ---------- | ------- | ------------- | ------- | -- |
| Stock      | 362         | 47%        | 20%     | 17%           | 8%      | 7% |
| Hall       | 1023        | 19%        | 40%     | 15%           | 18%     | 8% |
| Kitchen    | 220         | 53%        | 11%     | 13%           | 15%     | 8% |
| Pass       | 183         | 56%        | 9%      | 10%           | 15%     | 9% |
| Report     | 244         | 64%        | 7%      | 8%            | 12%     | 9% |
| Statistics | 1791        | 89%        | 1%      | 2%            | 2%      | 7% |

With pagination off, everything listed is on screen. A row's cost is the difference to the paged station over the difference in rows:

| station    | scale | listed | drawn   | per row | a second passes | per row | DOM nodes |
| ---------- | ----- | ------ | ------- | ------- | --------------- | ------- | --------- |
| Stock      | 1×    | 304    | 661.27  | 1.963   | 20.24           | 0.06    | 144136    |
| Stock      | 4×    | 1216   | 2269.57 | 1.8     | 95.99           | 0.075   | 641133    |
| Hall       | 1×    | 346    | 159.81  | 0.258   | 12.76           | 0.032   | 6655      |
| Hall       | 4×    | 1384   | 463.6   | 0.278   | 47.79           | 0.034   | 22999     |
| Kitchen    | 1×    | 20     | 80.82   | 1.478   | 2.97            | 0.126   | 6085      |
| Kitchen    | 4×    | 80     | 177.3   | 1.481   | 7.63            | 0.075   | 22345     |
| Pass       | 1×    | 10     | 48.91   | —       | 0.78            | —       | 922       |
| Pass       | 4×    | 40     | 74.06   | 0.615   | 2.1             | 0.03    | 2422      |
| Report     | 1×    | 220    | 92.45   | 0.159   | 6.42            | 0.023   | 2598      |
| Report     | 4×    | 877    | 222.26  | 0.192   | 23.36           | 0.025   | 9825      |
| Statistics | 1×    | 263    | 133.19  | 0.224   | 19.4            | 0.043   | 3442      |
| Statistics | 4×    | 1040   | 368.05  | 0.21    | 83.91           | 0.052   | 11212     |

### Correctness under stress: a random walk against the view model

3000 steps of real input (seed 20261120, 166 s): after every one the station on screen read exactly what the view model says of the reference model. The day ended with 310 orders, 288 of them paid.

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

9588 random clicks over 240 s under the clock, sampled every 25 after a forced collection. The slope is over the second half of the run:

|           | first sample | last sample | slope per minute |
| --------- | ------------ | ----------- | ---------------- |
| heap (MB) | 6.3          | 10.1        | 0.19             |
| DOM nodes | 7954         | 16979       | 310.61           |
| listeners | 872          | 2159        | 46.05            |

What one doing leaves behind for good (twenty of them between two forced collections, a station alone):

| done                      | DOM nodes | listeners | heap (KB) |
| ------------------------- | --------- | --------- | --------- |
| stockroom: a page turned  | -0.1      | 0         | 18.1      |
| hall: a page turned       | -0.1      | 0         | 15.4      |
| hall: a group picked      | 0         | 0         | 13.9      |
| hall: a table picked      | 0         | 0         | 4.6       |
| kitchen: a page turned    | -0.1      | 0         | 15.3      |
| report: a page turned     | -0.1      | 0         | 8.1       |
| statistics: a page turned | -0.1      | 0         | 8.2       |

900 station switches after a warm-up of twelve: heap 7.2 → 7.8 MB, DOM nodes 11623 → 11623, listeners 1275 → 1275.

The clock, watched for 60 s with the hall open: 0.9969 of its seconds pass in a second (each beat waits a full second after the one before has been handled). Across 7.2 s with the page frozen for 6 of them it counted 1.

No exception and no console warning.

### Application size: synthetic applications of growing width

Each station is a display, an editor, a keyed table of five rows and three buttons with their folds; one model, one knot. Compile time is for the two modules (view and view model), over the 0.3 s the compiler needs with nothing to do. Growth is the exponent against the number of stations.

|                                   | 6 stations | 12 stations | 24 stations | 48 stations | growth |
| --------------------------------- | ---------- | ----------- | ----------- | ----------- | ------ |
| compile (s)                       | 0.3        | 0.7         | 2.2         | 8.8         | 1.63   |
| bundle, minified (KB)             | 498.6      | 519.8       | 592.6       | 856.9       | 0.25   |
| bundle, gzipped (KB)              | 95.3       | 96.9        | 100.3       | 108.1       | 0.06   |
| drawn (ms)                        | 44.33      | 57.61       | 62.39       | 79.37       | 0.26   |
| DOM nodes                         | 966        | 1356        | 2136        | 3696        | 0.65   |
| a tick fed, one station open (ms) | 0.74       | 0.54        | 0.66        | 1.01        | 0.16   |
| a button clicked (ms)             | 1.36       | 1.62        | 1.17        | 1.54        | 0.01   |

No exception and no console warning.

