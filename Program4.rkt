#lang racket
;shopping list
(define shopList '("bread" "milk" "toilet paper" "cigarettes"))

;to display shopList
(define (displayShopList list)
  (for-each (lambda (arg)
              (printf "\t~a\n" arg))
            list))

;add item to a list
(define (addItem item lst)
  (cons item lst))

;remove item from a list
(define (removeItem item lst)
  (filter (lambda (x) (not (equal? x item))) lst))

;similar to displayShopList, but without newline
(define (displayList list)
  (for-each (lambda (arg)
              (printf " ~a" arg))
            list))

;Display and read function
(define (display&Read prompt)
  (display prompt)
  (read-line (current-input-port) 'any))

;List of items in aisles
(define aisle1 '("1" "eggs" "milk" "ice cream"))
(define aisle2 '("2" "strawberries" "bananas" "grapes"))
(define aisle3 '("3" "flour" "sugar" "yeast"))
(define aisle4 '("4" "paper plates" "napkins" "toilet paper"))
(define aisle5 '("5" "hotdogs" "ham" "chicken"))
(define aisle6 '("6" "carrots" "celery" "onions"))
(define aisle7 '("7" "chocholate bar" "cookies" "gum"))
(define aisle8 '("8" "bread" "cereal" "bagels"))
(define aisle9 '("9" "lottery ticket" "cigarettes" "zyns"))
(define aisle10 '("10" "legos" "doll" "hot wheels"))

;List of aisles
(define allAisles '(aisle1 aisle2 aisle3 aisle4 aisle5 aisle6 aisle7 aisle8 aisle9 aisle10))

;Aisle's prompt when player enters
(define (prompt aisle)
  (display "\nYou have entered Aisle ")
  (displayln (list-ref aisle 0))
  (display "This aisle has: ")
  (displayList (list-tail aisle 1)))
  


;function to play an aisle
(define (playAisle aisle playerItems)
  (prompt aisle)
  (let ([choice (display&Read "\nWould you like to pick up an item? (enter item name, or enter \"no\"):\n")])
    (cond
      [(equal? choice "no") playerItems]
      [(member choice aisle) 
       (let ([updatedAisle (removeItem choice aisle)])
         (addItem choice playerItems))]
      [else
       (displayln "Invalid choice.")
       (playAisle aisle playerItems)])))



;player's hand
(define playerItems '())


;Get next aisle to go to
(define (nextAisle aisle playerItems)
  (cond
        [(string-ci=? aisle "1") (playAisle aisle1  playerItems)]
        [(string-ci=? aisle "2") (playAisle aisle2  playerItems)]
        [(string-ci=? aisle "3") (playAisle aisle3  playerItems)]
        [(string-ci=? aisle "4") (playAisle aisle4  playerItems)]
        [(string-ci=? aisle "5") (playAisle aisle5  playerItems)]
        [(string-ci=? aisle "6") (playAisle aisle6  playerItems)]
        [(string-ci=? aisle "7") (playAisle aisle7  playerItems)]
        [(string-ci=? aisle "8") (playAisle aisle8  playerItems)]
        [(string-ci=? aisle "9") (playAisle aisle9  playerItems)]
        [(string-ci=? aisle "10") (playAisle aisle10 playerItems)]
        [(string-ci=? aisle "exit") (exitGame shopList playerItems)]
        [else (displayln "Invalid aisle, try again (1-10): ") (nextAisle (display&Read ""))]))

;Function to exit the store
;asked chatgpt what to use for if statement here, and it recommended "begin" statements
(define (exitGame shopList items)
  (displayln "You have purchased and walked out of the store with your gathered items, but have you gotten what you needed?")
  (if (equal? (sort shopList string<?) (sort items string<?))
      (begin
        (displayln "Congratulations! You took the groceries to your mom and she gave you a big hug and congratulated you for once!")
        (displayln "Thank you for playing!"))
      (begin
        (printf "You hand her the bag of groceries and she opens them to find")
        (displayList items)
        (displayln ".\nYou failed to bring home the 4 items your mom requested of you.")
        (displayln "She swiftly undoes her belt and whips you violently for a minute straight.")
        (displayln "Thanks for playing, and better luck next time pal!"))))
       

;Intro to game and first loop
(define (printOpening shopList)
  (displayln "Welcome to Shopping List!\n\nMom thinks you need to get out of the house for once.")
  (displayln "She hands you her grocery list and tells you to go to the market,")
  (displayln "threatening a beating if you fail to listen to her drunken commands.")
  (displayln "\n\t\tList\n")
  (displayShopList shopList)
  (displayln "\nUpon arriving to the market, you walk in the door and all 10 aisles are in view.")
  (displayln "Renovations are underway and the new aisle markers have not been installed, leaving")
  (displayln "you lost on where anything is located, especially after the shelves have been reorganized.")
  (nextAisle (display&Read "Where do you begin your search for the groceries? Aisle... (Enter 1-10).\n") playerItems))

;Loop game till user exits
;(define playGame (shopList playerItems allAisles) 
 ; (let ([newPlayerItems (playAisle (printOpening shopList) shopList playerItems)])
 ;   (let loop ([shopList playerItems allAisles])
  ;    [(

(define (playGame shopList playerItems)
  (let loop ([items playerItems])
    (let ([nextAisleInput (display&Read "From your current aisle, Where do you go next? (Enter the next aisle number or 'exit' to finish)\n")])
      (cond
        [(string-ci=? nextAisleInput "exit") 
         (exitGame shopList playerItems)]
        [else
         (loop (nextAisle nextAisleInput playerItems))]))))




(playGame shopList (printOpening shopList))



  