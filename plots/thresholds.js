// JSON dataset of federal poverty guidelines and state median income levels
// will be used to draw vertical lines at cliffs

const poverty_levels = ['a', 'b', 'c', 'd']
const poverty_text = ["130% FPG", "185% FPG",
                      "210% FPG", "85% SMI"]

const fpl = [{
   "name": "1 adult",
   "size": "1",
   "levels": {
     "a": Math.round(1041 * 1.3),
     "b": Math.round(1041 * 1.85),
     "c": Math.round(1041 * 2.1),
     "d": 2578
   }
 }, {
   "name": "1 adult, 1 child",
   "size": "2",
   "levels": {
     "a": Math.round(1409 * 1.3),
     "b": Math.round(1409 * 1.85),
     "c": Math.round(1409 * 2.1),
     "d": 3372
   }
 }, {
   "name": "1 adult, 2 children",
   "size": "3",
   "levels": {
     "a": Math.round(1778 * 1.3),
     "b": Math.round(1778 * 1.85),
     "c": Math.round(1778 * 2.1),
     "d": 4165
   }
 }, {
   "name": "1 adult, 3 children",
   "size": "4",
   "levels": {
     "a": Math.round(2146 * 1.3),
     "b": Math.round(2146 * 1.85),
     "c": Math.round(2146 * 2.1),
     "d": 4958
   }
 }, {
   "name": "2 adults",
   "levels": {
     "a": Math.round(1409 * 1.3),
     "b": Math.round(1409 * 1.85),
     "c": Math.round(1409 * 2.1),
     "d": 3372
   }
 }, {
   "name": "2 adults, 1 child",
   "levels": {
     "a": Math.round(1778 * 1.3),
     "b": Math.round(1778 * 1.85),
     "c": Math.round(1778 * 2.1),
     "d": 4165
   }
 }, {
   "name": "2 adults, 2 children",
   "levels": {
     "a": Math.round(2146 * 1.3),
     "b": Math.round(2146 * 1.85),
     "c": Math.round(2146 * 2.1),
     "d": 4958
   }
 }, {
   "name": "2 adults, 3 children",
   "size": "5",
   "levels": {
     "a": Math.round(2514 * 1.3),
     "b": Math.round(2514 * 1.85),
     "c": Math.round(2514 * 2.1),
     "d": 5752
   }
 }, {
   "name": "6 people",
   "size": "6",
   "levels": {
     "a": Math.round(2882 * 1.3),
     "b": Math.round(2882 * 1.85),
     "c": Math.round(2882 * 2.1),
     "d": 6545
   }
 }];
