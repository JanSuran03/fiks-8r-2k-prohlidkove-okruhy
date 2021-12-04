(ns prohlidkove-okruhy.core
  (:require [clojure.string :as str]
            [loom.graph :refer :all]
            [loom.alg :as alg]))
;;  [clojure.string :as str] načte namespace clojure.str, na nějž můžu odkazovat jako str/<něco>
;;  [loom.graph :refer :all] mi umožní přímo použít všechny nadefinované funkce/konstanty z namespacu loom.graph.

;;  Důležité body obecně:
;;  1) Prefixní notace: (+ 1 2 3)   =   1 + 2 + 3
;;  2) Clojure je dynamicky typovaný (nápovědy pro compiler jsou ve více než 99 % případů zbytečné).
;;  3) Clojure používá vytrvalé imutabilní datové struktury. To nám umožňuje v téměř konstantním čase kopírovat datové
;;     struktury a extrémně efektivně s nimi pracovat, aniž bychom je měnili na místě.
;;  2/3) Proto nikde neuvidíte nic jako "public static int i = ...;" - umožňuje mi to "kódit" mnohem efektivněji.
;;
;;  (defn fn-name [<args>]
;;     ...
;;     ...
;;     xyz)
;;  (forma vždy vrací poslední hodnotu)
;;
;;  je stejné jako v Pythonu
;;  def fn-name(<args>):
;;      ...
;;      ...
;;      return xyz
;;
;;  Threading macro (->>) funguje následujícím způsobem: Zhodnotí první formu/funkci, její
;;  výsledek dá jako poslední argument do druhé funkce, to jako poslední argument do další atd.
;;
;;  slurp přečte soubor a vrátí jej jako string.
;;
;;  str/split-lines rozdělí vstupní soubor podle \n nebo \r.
;;
;;  (map f coll) aplikuje funkci f na každý prvek kolekce a vrátí novou (línou) sekvenci, např.
;;  (map inc [4 2 0]) => (5 3 1)
;;  (po vyprintování je ohraničená závorkami), viz (map read-string ...) níže
;;
;;  #(... % ...) značí anonymní funkci. Konkrétně tato funkce se dá přepsat jako:
;;  (fn [arg-1]
;;    (... arg-1 ...))
;;
;;  as-> threading macro funguje takto: první argument (v tomto případě) nahradíme symbolem <>;
;;  zhodnotíme první formu a ta se stane novou hodnotou symbolu <>. A takto postupujeme dál
;;  a dál... nakonec funkce vrátí hodnotu symbolu <>.
;;
;;  str/split rozdělí string podle \"regular expression\", která je převzatá z Javy. Vrátí
;;  vektor (pole), např. (str/split "a b c" #"\ ") => ["a" "b" "c"]
;;
;;  Protože jsou všechny argumenty čísla, všechny stringy vklidu přečíst jako 3 čísla (integery):
;;  (map read-string ["1" "2" "3"]) => (1 2 3)
;;
;;  Návratová hodnota funkce může vypadat např. takto:
;;  ((10 2 3) (4 5) (6 7) (8 9) (10 11) (12 13)) ... řekněme, že toto je nyní `output`.
;;  Clojure má velice vychytaný tzv. destructuring. To nám umožní v `let` makru lokálně přiřadit
;;  více symbolům danou hodnotu:
;;  [něco & input] = output   znamená, že něco je první prvek outputu a zbytek označíme jako ìnput
;;  [_num-crossroads num-edges _num-queries] něco   znamená, že _num-crossroads je první, num-edges
;;  druhý a _num-queries třetí prvek něčeho. Je zvyk, že nepoužité symboly začínají podtržítkem.
;;  jak nyní vypadá stav lokálních vazeb:
;;  ((4 5) (6 7) (8 9) (10 11) (12 13)) ... input
;;  10 ... _num-crossroads
;;  2 ... num-edges
;;  3 ... _num-queries
;;
;;  funkce (split-at n coll) rozdělí kolekci na dvě části:
;;  [(take n coll) (drop n coll)]

(defn read-and-process-input []
  (let [[[_num-crossroads num-edges _num-queries] & input]
        (->> "input.txt" slurp
             str/split-lines
             (map #(as-> % <>
                         (str/split <> #"\ ")
                         (map read-string <>))))]
    (split-at num-edges input)))

;;  Funkce path-exists využívá funkce z knihovny https://github.com/aysylu/loom, Clojure knihovny pro
;;  grafové operace.
;;  Je to prosté - pokud cesta existuje, (alg/bf-path graph vertex-1 vertex-2) nám tuto cestu vrátí,
;;  jinak nám vrátí nil; nil & false jsou v Clojure logické nepravdy. Podle toho nám bude navrácen
;;  vhodný string.

(defn path-exists? [graph [vertex-1 vertex-2 :as _query]]
  (if (alg/bf-path graph vertex-1 vertex-2)
    "Cesta existuje"
    "Cesta neexistuje"))

;;  Pomocí destructuringu si označíme edges & queries jako první & druhý prvek výstupové hodnoty
;;  volání funkce (read-and-process-input) s 0 argumenty.
;;  graph je náš graf. Funkce digraph (directed graph = orientovaný nehodnocený graf) bere dvojice vrcholů.
;;  (apply digraph [[1 2] [3 4]]) je identické s (digraph [1 2] [3 4]).
;;  pro každý dotaz (dvojice vrcholů) zjistíme, zda cesta existuje. Tyto návratové stringy spojíme
;;  znakem \n (newline) a funkce spit tento string vypíše do souboru "output.txt".

(defn -main [& _args]
  (let [[edges queries] (read-and-process-input)
        graph (apply digraph edges)]
    (->> queries (map #(path-exists? graph %))
         (str/join "\n")
         (spit "output.txt"))))