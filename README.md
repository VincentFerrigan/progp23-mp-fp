# Mästarprov Funktionell Programmering
# progp23-mp-fp

## 1. En lista med heltal
Implementera en funktion i Haskell som tar in en lista av heltal och returnerar
en lista där varje element är kvadraten av det ursprungliga elementet, förutom
de element som är negativa eller 0. Icke-positiva element ska istället
ignoreras och inte finnas med i den resulterande listan.
**Exempelkörningar:**
```
prelude> :l list_of_numbers.hs
[1 of 1] Compiling Main        ( list_of_numbers.hs, interpreted )
Ok, one module loaded.
*Main> sq
sqrt     squarePositive
*Main> squarePositive [-2, -1, 0, 1, 2]
[1,4]
*Main> squarePositive [1, 3, 2]
[1, 9, 4]
*Main> squarePositive [0, 0, 0, 0]
[]
*Main> squarePositive [-3, -5, -8, 2]
[4]
```

Implementationen av squarePositive behöver uppfylla följande krav:

1. Du behöver använda högre ordningens funktioner för att implementera
   funktionen. 
   
Dokumentationen av squarePositive behöver uppfylla följande krav:

1. Redogör för vilka högre ordningens funktioner som du använder och förklara
   varför dessa är högre ordningens funktioner.
2. Redogör för hur funktionen använder immutability.

## 2. Resmål
Definiera ett *Record* för enskilda personer kallat *Person* som har namn, ålder
och en lista över strängar som representerar resmål. 

Skriv sedan en funktion som läser in en lista över Person och returnerar en
lista över alla resmål som de tillsammans har besökt med alla resmål som
besökts av minst en person i listan. 

Kravet på listan är att alla resmål ska vara unika.
Uppgiften går att lösa effektivt men det är inget krav. En lösning som är
O(n^3) ger godkänt.

**Dokumentationskrav:** Dokumentera utförligt hur man skapar en instans av Person.

Ledning: Learn You a Haskell har en [guide till hur du använder Record Syntax](http://learnyouahaskell.com/making-our-own-types-and-typeclasses).

## 3. Svansrekursiv sifferkedja
En sifferlek går till på följande sätt. Du börjar på ett tal current och ska nå
ett tal goal. Dina två tillåtna drag är:
1. Att lägga till 2 till talet
2. Att dra ifrån 3 från talet.

Leken tar slut då du når goal. Leken ställer krav på att du loggar alla
delresultat på vägen. Det finns alltid minst en giltig lösning.
**Exempelkörningar:**
```
*Main> :l numberChain.hs
[1 of 1] Compiling Main        ( numberChain.hs, interpreted )
Ok, one module loaded.
*Main> numberChain 3 16
[3, 5, 7, 9, 11, 13, 15, 17, 14, 16]
*Main> numberChain 1 2
[1, 3, 0, 2]
*Main> numberChain 1 1
[1]
*Main> numberChain 17 42
[17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 40, 42]
```
Du behöver också kunna hantera negativa tal. Gör detta genom att skriva talet
inom parentes så att Haskell förstår att det är unärt minus.
```
*Main> numberChain (-1) (-10)
[-1, -4, -7, -10]
*Main> numberChain (-1) (-9)
[-1, -4, -7, -10, -8, -11, -9]
```

**Ledning:**

1. En girig algoritm som alltid går mot målet när den kan och sedan antingen
   backar eller skjuter över målet ger en optimal lösning.

**Krav på implementationen:**

1. Du behöver använda en hjälpfunktion.
2. Högre ordningens funktioner är förbjudna här.
3. Hjälpfunktionen behöver vara implementerad med hjälp av svansrekursion.
4. Din lösning får inte använda slump.

**Krav på dokumentationen:**
1. Skriv vad som gör din implementation svansrekursiv.

## 4. Kortaste vägen
Skriv en funktion shortestpath som tar in en lista med koordinater i planet och
beräknar den kortaste vägen som går igenom alla punkter i den ordning som de
var givna i listan.
**Exempelkörning:**
```
*Main> totalDistance [(1.3, 2.4), (5.3, -1.3), (-4.2, -3.4), (5.2, 8.0)]
29.953845823305446
*Main> totalDistance [(1.3, 2.4), (5.3, -1.3)]
5.448853090330111
*Main> totalDistance [(5.3, -1.3), (-4.2, -3.4)]
9.729337079164232
*Main> totalDistance [(-4.2, -3.4), (5.2, 8.0)]
14.775655653811103
```

Tänk också på dessa specialfall:
```
*Main> totalDistance []
0.0
*Main> totalDistance [(3.24, 14.23)]
0.0
```

**Ledning:**
1. Avståndet mellan två punkter kan beräknas med Pythagoras sats.
2. Testa med exemplet ovan, som ges i lättpastead text här: 
   [(1.3, 2.4), (5.3, -1.3), (-4.2, -3.4), (5.2, 8.0)]

**Dokumentations- och implementationskrav:**
1. Impementera uppgiften med rena funktioner och dokumentera vad som gör dem rena.
2. Implementera uppgiften med referenstransparens och dokumentera vad som gör
   koden referenstransparent.
