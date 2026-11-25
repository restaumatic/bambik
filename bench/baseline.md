# bambik on Diner — 2026-11-25, 32a6f1af

11th Gen Intel(R) Core(TM) i7-11850H @ 2.50GHz, 16 threads, 63 GB; Chrome/158.0.8127.88; node v25.6.0; 1333 s.

### View model in node: one feed's reads per station (ms)

| station    | 1×    | 4×    | 16×    | 64×     | growth |
| ---------- | ----- | ----- | ------ | ------- | ------ |
| Magazyn    | 0.436 | 1.776 | 8.259  | 32.775  | 1.05   |
| Sala       | 0.444 | 1.988 | 8.383  | 35.356  | 1.05   |
| Kuchnia    | 0.063 | 0.315 | 1.773  | 9.309   | 1.21   |
| Wydawka    | 0.029 | 0.112 | 0.51   | 2.429   | 1.07   |
| Raport     | 0.139 | 0.673 | 2.905  | 14.143  | 1.11   |
| Statystyki | 0.718 | 3.547 | 19.713 | 104.541 | 1.2    |

The costliest reads at the largest scale:

| read                    | 1×    | 4×    | 16×    | 64×     | growth |
| ----------------------- | ----- | ----- | ------ | ------- | ------ |
| Statystyki: soughtSales | 0.698 | 3.47  | 19.402 | 103.255 | 1.21   |
| Magazyn: stockPage      | 0.2   | 0.803 | 4.545  | 17.528  | 1.09   |
| Sala: menuPage          | 0.173 | 0.867 | 4.108  | 16.545  | 1.1    |
| Raport: paidPage        | 0.06  | 0.327 | 1.568  | 8.311   | 1.18   |
| Magazyn: isLastPage     | 0.057 | 0.287 | 0.901  | 3.965   | 1      |
| Sala: seatings          | 0.037 | 0.161 | 0.799  | 3.902   | 1.13   |
| Sala: isFirstPage       | 0.055 | 0.228 | 0.812  | 3.832   | 1.01   |
| Magazyn: searchState    | 0.068 | 0.231 | 0.971  | 3.788   | 0.98   |
| Magazyn: pageLine       | 0.056 | 0.232 | 0.921  | 3.787   | 1.01   |
| Magazyn: isFirstPage    | 0.055 | 0.222 | 0.92   | 3.707   | 1.01   |

Generating the day (`dayAt`): 1× 2.1 ms, 4× 7.31 ms, 16× 29.25 ms, 64× 95.89 ms.

### Responsiveness: what one doing costs the browser (ms)

Everything the main thread does from the doing to the frame that shows it; the split is the median at full speed, the rest being painting, compositing and the browser's own — a frame with nothing done is its floor. The budget is on the 95th percentile at a quarter of the speed: 100 ms for something done, 16 ms for a second passing, 1 s for the start.

| done                                     | n  | p50   | p95   | script / style / layout / rest | p50 at ¼ | p95 at ¼ | budget   |
| ---------------------------------------- | -- | ----- | ----- | ------------------------------ | -------- | -------- | -------- |
| stock: a portion more                    | 30 | 8.73  | 13.47 | 2.55 / 0.54 / 0.31 / 5.31      | 32.54    | 55.02    | within   |
| stock: a portion fewer                   | 30 | 8.75  | 13.05 | 2.61 / 0.55 / 0.31 / 5.4       | 33.33    | 49.56    | within   |
| stock: a key in a portion count          | 30 | 5.82  | 8.6   | 2.3 / 0.16 / 0.24 / 3.16       | 22.26    | 29.54    | within   |
| stock: a key in a price                  | 30 | 5.3   | 6.43  | 2.2 / 0 / 0.22 / 2.8           | 20.82    | 24.68    | within   |
| stock: a page turned                     | 30 | 31.71 | 44.19 | 12.79 / 4.68 / 2.59 / 12.4     | 139.96   | 190.35   | over 100 |
| stock: a group picked                    | 30 | 33.98 | 46.76 | 11.65 / 6.07 / 2.44 / 14.43    | 145.76   | 190.07   | over 100 |
| stock: a key in the search               | 32 | 7.72  | 27.52 | 3.21 / 0.29 / 0.31 / 3.79      | 28.68    | 112.22   | over 100 |
| hall: a dish added                       | 30 | 8.51  | 12.94 | 4.25 / 0.33 / 0.43 / 3.52      | 27.21    | 48.74    | within   |
| hall: a portion more on a line           | 30 | 9.8   | 11.43 | 4.03 / 0.82 / 0.54 / 4.28      | 41.27    | 55.13    | within   |
| hall: a portion fewer on a line          | 30 | 10.36 | 12.8  | 4.08 / 1 / 0.57 / 4.77         | 42.92    | 53.67    | within   |
| hall: a table picked                     | 30 | 9.1   | 12.47 | 4.22 / 1.18 / 0.82 / 3.31      | 37.99    | 45.53    | within   |
| hall: a page turned                      | 30 | 12.26 | 14.14 | 5.91 / 1.21 / 0.83 / 4.35      | 49.36    | 63.38    | within   |
| hall: a group picked                     | 30 | 13.64 | 15.66 | 5.92 / 2.34 / 0.81 / 4.49      | 55.07    | 76.66    | within   |
| hall: a key in the search                | 32 | 8.35  | 18.33 | 4.58 / 0.43 / 0.72 / 2.84      | 36.22    | 49.28    | within   |
| kitchen: a page turned                   | 30 | 20.77 | 29.57 | 9.52 / 2.93 / 1.64 / 7.06      | 82.33    | 127.51   | over 100 |
| kitchen: a line plated                   | 7  | 6.77  | 9.65  | 2.44 / 0.68 / 0.36 / 3.12      | 31.17    | 57.32    | within   |
| kitchen: an order passed on, asked       | 12 | 10.05 | 15.01 | 0.48 / 4.21 / 0.38 / 4.91      | 34.15    | 54.25    | within   |
| kitchen: an order passed on, confirmed   | 12 | 38.89 | 56.28 | 3.85 / 11.2 / 2.97 / 21.14     | 114.36   | 156.4    | over 100 |
| pass: a payment, asked                   | 10 | 6.46  | 10.72 | 0.57 / 2.4 / 0.42 / 3.32       | 25.76    | 44.76    | within   |
| pass: a payment, confirmed               | 10 | 26.02 | 27.06 | 2.13 / 7.51 / 1.71 / 14.5      | 66.29    | 75.86    | within   |
| report: a page turned                    | 30 | 6.9   | 8.93  | 2.47 / 0.89 / 0.6 / 2.97       | 22.42    | 36.87    | within   |
| statistics: a page turned                | 30 | 7.64  | 9.43  | 2.97 / 0.85 / 0.57 / 3.25      | 25.47    | 32.32    | within   |
| statistics: a group picked               | 30 | 9.31  | 15.3  | 3.48 / 1.8 / 0.59 / 3.44       | 33.84    | 42.5     | within   |
| statistics: a key in the search          | 32 | 6.23  | 9.27  | 3.18 / 0.2 / 0.61 / 2.12       | 20.25    | 31.12    | within   |
| app: a frame with nothing done           | 6  | 0.86  | 1.5   | 0.05 / 0.13 / 0 / 0.68         | 2.08     | 2.66     | within   |
| app: a second passes at Magazyn          | 6  | 1.91  | 7.23  | 0.25 / 0.14 / 0 / 1.96         | 5.11     | 6.84     | within   |
| app: a second passes at Sala             | 6  | 1.55  | 1.62  | 0.25 / 0.13 / 0 / 1.17         | 4.09     | 5.13     | within   |
| app: a second passes at Kuchnia          | 6  | 1.93  | 8.4   | 0.47 / 0.08 / 0 / 1.39         | 5.28     | 8.7      | within   |
| app: a second passes at Wydawka          | 6  | 1.69  | 2.23  | 0.47 / 0.11 / 0 / 1.33         | 5.57     | 21.04    | over 16  |
| app: a second passes at Raport           | 6  | 1.54  | 2.38  | 0.35 / 0.11 / 0 / 1.12         | 3.48     | 6.75     | within   |
| app: a second passes at Statystyki       | 6  | 1.45  | 2.27  | 0.21 / 0.08 / 0 / 1.33         | 4.73     | 6.63     | within   |
| app: switched to Magazyn                 | 20 | 26.39 | 45.15 | 5.16 / 8.26 / 2.6 / 10.5       | 127.94   | 146      | over 100 |
| app: switched to Sala                    | 20 | 24.04 | 40.27 | 6.73 / 5.54 / 1.73 / 9.81      | 136.48   | 147.75   | over 100 |
| app: switched to Kuchnia                 | 20 | 16.59 | 26.29 | 3.15 / 5.39 / 1.59 / 6.32      | 78.84    | 97.34    | within   |
| app: switched to Wydawka                 | 20 | 11.42 | 13.38 | 1.93 / 3.16 / 0.94 / 5.62      | 45.99    | 57.04    | within   |
| app: switched to Raport                  | 20 | 9.1   | 11.08 | 2.12 / 2.58 / 0.66 / 4.01      | 33.24    | 47.19    | within   |
| app: switched to Statystyki              | 20 | 13.71 | 17.54 | 4.22 / 4.12 / 1.03 / 4.38      | 56.98    | 63.73    | within   |
| app: started, until it waits for its day | 6  | 49.53 | 53.23 | 39.17 / 1.21 / 0.17 / 9.35     | 202.94   | 211.85   | within   |
| app: started, the day drawn              | 6  | 73.46 | 80.25 | 41.31 / 4.27 / 12.86 / 16.28   | 272.95   | 286.01   | within   |

Where the time of a station's doings goes (CPU profile, self time by layer):

| station    | ms profiled | view model | carrier | design system | browser | gc |
| ---------- | ----------- | ---------- | ------- | ------------- | ------- | -- |
| Stock      | 1601        | 5%         | 20%     | 31%           | 40%     | 4% |
| Hall       | 961         | 14%        | 32%     | 7%            | 42%     | 5% |
| Kitchen    | 821         | 2%         | 16%     | 23%           | 55%     | 4% |
| Pass       | 314         | 1%         | 3%      | 16%           | 78%     | 1% |
| Report     | 109         | 5%         | 23%     | 6%            | 62%     | 4% |
| Statistics | 355         | 22%        | 19%     | 12%           | 42%     | 5% |

Stock, the functions that took most: (program) 627.59 ms; scrollToTab (node_modules/@material/web/tabs/internal/tabs.js) 79.47 ms; (garbage collector) 63.79 ms; showStringImpl (Data.Show) 34.21 ms; insertBefore 30.53 ms.
Hall, the functions that took most: (program) 382.37 ms; (garbage collector) 46.68 ms; (anonymous) (Effect) 36.29 ms; __do3 (PUI) 24.87 ms; setAttribute 23.94 ms.
Kitchen, the functions that took most: (program) 439.69 ms; animate 45.84 ms; (garbage collector) 31.82 ms; showModal 31.66 ms; (anonymous) (Effect) 12.63 ms.
Pass, the functions that took most: (program) 237.55 ms; animate 17.82 ms; showModal 15.71 ms; (anonymous) 2.78 ms; (garbage collector) 2.05 ms.
Report, the functions that took most: (program) 65.26 ms; (garbage collector) 4.58 ms; setAttribute 4.08 ms; (anonymous) (Effect) 3.73 ms; mergeFromTo (Data.Array) 1.06 ms.
Statistics, the functions that took most: (program) 144.07 ms; (garbage collector) 17.99 ms; (anonymous) (Data.Ord) 16.25 ms; getAnimations 10.12 ms; (anonymous) (Data.Map.Internal) 5.69 ms.

No exception and no console warning.

### Large data: a station alone on days of growing load (ms)

The standard day is 1×; 64× is 19456 dishes, 2688 tables and 16128 orders. Growth is the exponent k of cost ≈ c·scale^k: 0 is flat, 1 linear, above 1 a finding. A cost is everything the main thread did up to the frame that shows the change, painting and compositing included; a second of a station with no clock is a frame with nothing done.

| station    | measured        | 1×     | 4×     | 16×    | 64×    | growth |
| ---------- | --------------- | ------ | ------ | ------ | ------ | ------ |
| Stock      | drawn           | 105    | 135.21 | 163.28 | 294.47 | 0.24   |
| Stock      | a second passes | 0.57   | 0.65   | 0.71   | 0.56   | 0      |
| Stock      | a portion more  | 9.83   | 14.76  | 30.12  | 89.88  | 0.53   |
| Stock      | DOM nodes       | 6184   | 6192   | 6192   | 6192   | 0      |
| Stock      | heap (MB)       | 6.3    | 5.9    | 10.4   | 17.2   | 0.26   |
| Hall       | drawn           | 106.85 | 152.64 | 314.56 | 883.79 | 0.51   |
| Hall       | a second passes | 1.36   | 1.65   | 2.32   | 1.58   | 0.06   |
| Hall       | a dish added    | 8.67   | 12.98  | 38.74  | 130.06 | 0.66   |
| Hall       | DOM nodes       | 2457   | 5121   | 15777  | 54027  | 0.75   |
| Hall       | heap (MB)       | 7.2    | 11.9   | 18.8   | 51.6   | 0.46   |
| Kitchen    | drawn           | 95.4   | 93.38  | 116.92 | 195.11 | 0.17   |
| Kitchen    | a second passes | 1.44   | 1.51   | 1.55   | 1.24   | -0.03  |
| Kitchen    | a page turned   | 21.85  | 23.03  | 27     | 31.48  | 0.09   |
| Kitchen    | DOM nodes       | 3286   | 3143   | 3325   | 2896   | -0.02  |
| Kitchen    | heap (MB)       | 4.9    | 7.5    | 7.5    | 13.4   | 0.22   |
| Pass       | drawn           | 63.12  | 71.23  | 91.26  | 148.02 | 0.2    |
| Pass       | a second passes | 1.39   | 1.14   | 1.06   | 1.34   | -0.01  |
| Pass       | a payment taken | 34.89  | 34.98  | 38.03  | 46.04  | 0.07   |
| Pass       | DOM nodes       | 922    | 924    | 924    | 924    | 0      |
| Pass       | heap (MB)       | 4.7    | 4.7    | 6.3    | 15.6   | 0.28   |
| Report     | drawn           | 58.34  | 68.57  | 92.34  | 165.77 | 0.25   |
| Report     | a second passes | 1.05   | 1.1    | 1.1    | 0.93   | -0.03  |
| Report     | a page turned   | 6.81   | 8.08   | 10.21  | 21.05  | 0.26   |
| Report     | DOM nodes       | 301    | 301    | 301    | 301    | 0      |
| Report     | heap (MB)       | 4.4    | 5.1    | 7.7    | 20.5   | 0.36   |
| Statistics | drawn           | 81.5   | 90.14  | 166.77 | 384.36 | 0.38   |
| Statistics | a second passes | 0.79   | 0.72   | 0.77   | 0.6    | -0.05  |
| Statistics | a page turned   | 8.58   | 11.44  | 27.19  | 107.08 | 0.61   |
| Statistics | DOM nodes       | 924    | 924    | 924    | 924    | 0      |
| Statistics | heap (MB)       | 4.4    | 5.2    | 9.5    | 25.6   | 0.42   |

Where drawing the day at 64× goes (CPU profile, self time by layer):

| station    | ms profiled | view model | carrier | design system | browser | gc  |
| ---------- | ----------- | ---------- | ------- | ------------- | ------- | --- |
| Stock      | 371         | 48%        | 19%     | 18%           | 8%      | 6%  |
| Hall       | 1106        | 19%        | 41%     | 14%           | 17%     | 9%  |
| Kitchen    | 223         | 52%        | 11%     | 13%           | 16%     | 8%  |
| Pass       | 231         | 57%        | 10%     | 11%           | 13%     | 9%  |
| Report     | 206         | 59%        | 8%      | 7%            | 13%     | 12% |
| Statistics | 414         | 75%        | 4%      | 7%            | 5%      | 8%  |

With pagination off, everything listed is on screen. A row's cost is the difference to the paged station over the difference in rows:

| station    | scale | listed | drawn   | per row | a second passes | per row | DOM nodes |
| ---------- | ----- | ------ | ------- | ------- | --------------- | ------- | --------- |
| Stock      | 1×    | 304    | 942.54  | 2.849   | 0.77            | 0.001   | 144136    |
| Stock      | 4×    | 1216   | 3250.01 | 2.583   | 1.34            | 0.001   | 641133    |
| Hall       | 1×    | 346    | 183.05  | 0.272   | 1.7             | 0.001   | 6655      |
| Hall       | 4×    | 1384   | 495.21  | 0.287   | 2.32            | 0.001   | 22999     |
| Kitchen    | 1×    | 20     | 104.22  | 0.882   | 1.72            | 0.028   | 6085      |
| Kitchen    | 4×    | 80     | 232.48  | 1.987   | 4.21            | 0.039   | 22345     |
| Pass       | 1×    | 10     | 63.06   | —       | 1.32            | —       | 922       |
| Pass       | 4×    | 40     | 87.45   | 0.541   | 2.24            | 0.037   | 2422      |
| Report     | 1×    | 220    | 108.59  | 0.24    | 2.23            | 0.006   | 2598      |
| Report     | 4×    | 877    | 253.54  | 0.214   | 4.58            | 0.004   | 9825      |
| Statistics | 1×    | 263    | 131.82  | 0.2     | 0.63            | -0.001  | 3442      |
| Statistics | 4×    | 1040   | 321     | 0.224   | 0.68            | 0       | 11212     |

### Correctness under stress: a random walk against the view model

3000 steps of real input (seed 20261120, 156 s): after every one the station on screen read exactly what the view model says of the reference model. The day ended with 310 orders, 288 of them paid.

Done along the way: open ×1177, next page ×326, previous page ×239, add ×186, search ×162, order ×159, serve table ×136, plate ×125, stock + ×76, stock − ×64, price of ×61, group ×60, portions of ×53, line + ×48, zero ×45, send ×41, remove ×27, line − ×15.

No exception and no console warning.

### Races: the application driven faster than a person can

- holds: twelve clicks on a dish with no wait between them put twelve portions on the order
- holds: three digits typed while 4 seconds pass stay as typed, the caret after them
- holds: and are the stock when the stockroom is come back to
- holds: a dish clicked while the order was on its way went with it or stayed in stock, never both or neither
- holds: the last page losing its only row falls back to the page before
- holds: a payment confirmed twice at once is taken once

No exception and no console warning.

### Reliability: left running

10047 random clicks over 240 s under the clock, sampled every 25 after a forced collection. The slope is over the second half of the run:

|           | first sample | last sample | slope per minute |
| --------- | ------------ | ----------- | ---------------- |
| heap (MB) | 6.4          | 9.8         | 0.25             |
| DOM nodes | 7957         | 13668       | 213.85           |
| listeners | 873          | 1486        | 20.74            |

What one doing leaves behind for good (twenty of them between two forced collections, a station alone):

| done                      | DOM nodes | listeners | heap (KB) |
| ------------------------- | --------- | --------- | --------- |
| stockroom: a page turned  | -0.1      | 0         | 18.6      |
| hall: a page turned       | -0.1      | 0         | 15.8      |
| hall: a group picked      | 0         | 0         | 14.4      |
| hall: a table picked      | 0         | 0         | 4.6       |
| kitchen: a page turned    | -0.1      | 0         | 16        |
| report: a page turned     | -0.1      | 0         | 8.5       |
| statistics: a page turned | -0.1      | 0         | 8.7       |

900 station switches after a warm-up of twelve: heap 7.3 → 7.9 MB, DOM nodes 11626 → 11626, listeners 1276 → 1276.

The clock, watched for 60 s with the hall open: 1 of its seconds pass in a second. Across 7.2 s with the page frozen for 6 of them it counted 0.

No exception and no console warning.

### Application size: synthetic applications of growing width

Each station is a display, an editor, a keyed table of five rows and three buttons with their folds; one model, one knot. Compile time is for the two modules (view and view model), over the 0.2 s the compiler needs with nothing to do. Growth is the exponent against the number of stations.

|                                   | 6 stations | 12 stations | 24 stations | 48 stations | growth |
| --------------------------------- | ---------- | ----------- | ----------- | ----------- | ------ |
| compile (s)                       | 0.2        | 0.7         | 2.1         | 8.3         | 1.77   |
| bundle, minified (KB)             | 500.1      | 521.3       | 594         | 858.4       | 0.25   |
| bundle, gzipped (KB)              | 95.8       | 97.5        | 100.9       | 108.7       | 0.06   |
| drawn (ms)                        | 58.23      | 68.35       | 74.73       | 97.28       | 0.23   |
| DOM nodes                         | 966        | 1356        | 2136        | 3696        | 0.65   |
| a tick fed, one station open (ms) | 1.46       | 1.62        | 1.65        | 1.61        | 0.04   |
| a button clicked (ms)             | 3.51       | 3.86        | 3.9         | 5.46        | 0.19   |

No exception and no console warning.

