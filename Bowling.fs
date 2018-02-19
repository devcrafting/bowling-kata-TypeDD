module Bowling

type Frame =
    | Strike
    | Spare of firstRoll:NbPins
    | Roll of firstRoll:NbPins * secondRoll:NbPins
and NbPins = Miss | One | Two | Three | Four | Five | Six | Seven | Eight | Nine

type LastFrame =
    | LastStrike of firstBonusBall:BonusBallNbPins * secondBonusBall:BonusBallNbPins
    | LastSpare of firstRoll:NbPins * bonusBall:BonusBallNbPins
    | LastRoll of firstRoll:NbPins * secondRoll:NbPins
and BonusBallNbPins = NbPins of NbPins | All

type Game = private Game of Frame list * LastFrame

module Game = 
    let private nbPinsFrom = function
        | '-' -> Miss
        | '1' -> One
        | '3' -> Three
        | '5' -> Five
        | '9' -> Nine
    let private bonusBallNbPinsFrom = function
        | 'X' -> All
        | nb -> NbPins <| nbPinsFrom nb
    let rec private create' frames lastFrame = function
        | ['X'; 'X'; 'X'] ->
            frames, Some <| LastStrike (All, All)
        | [firstRoll; '/'; nb] ->
            frames, Some <| LastSpare (nbPinsFrom firstRoll, bonusBallNbPinsFrom nb)
        | [firstRoll; secondRoll] -> 
            frames, Some <| LastRoll (nbPinsFrom firstRoll, nbPinsFrom secondRoll)
        | 'X'::tail ->
            let frames = frames @ [Strike]
            create' frames lastFrame tail
        | firstRoll::'/'::tail ->
            let frames = frames @ [Spare <| nbPinsFrom firstRoll]
            create' frames lastFrame tail
        | firstRoll::secondRoll::tail -> 
            let frames = frames @ [Roll (nbPinsFrom firstRoll, nbPinsFrom secondRoll)] 
            create' frames lastFrame tail
        | _ -> failwith "Unknown rolls"
    let create array =
        let (frames, Some lastFrame) = create' [] None array
        frames, lastFrame

let toInt = function
    | Miss -> 0
    | One -> 1
    | Two -> 2
    | Three -> 3
    | Five -> 5
let toInt' = function
    | NbPins nb -> toInt nb
    | All -> 10

let score game =
    let frames, lastFrame = game
    let lastFrameScore, lastFrameFirstBall, lastFrameSecondBall =
        match lastFrame with
        | LastStrike (bonus1, bonus2) -> 10 + toInt' bonus1 + toInt' bonus2, 10, toInt' bonus1
        | LastSpare (fst, bonus) -> 10 + toInt' bonus, toInt fst, 10 - toInt fst 
        | LastRoll (fst, snd) -> toInt fst + toInt snd, toInt fst, toInt snd 
    let rec score' acc = function
        | Strike::tail -> 
            let nextRoll, nextNextRoll, nextFrames =
                match tail with
                | Strike::Strike::_ -> 10, 10, tail
                | Strike::Spare fst::_
                | Strike::Roll (fst, _)::_ -> 10, toInt fst, tail
                | [Strike] -> 10, lastFrameFirstBall, []
                | Spare _::_ -> 10, 0, tail
                | Roll (fst, snd)::_ -> toInt fst, toInt snd, tail
                | [] -> lastFrameFirstBall, lastFrameSecondBall, tail
            score' (acc + 10 + nextRoll + nextNextRoll) nextFrames            
        | Spare _::tail ->
            let nextRoll =
                match tail with
                | Strike::_ -> 10
                | Spare fst::_
                | Roll (fst, _)::_ -> toInt fst
                | [] -> lastFrameFirstBall
            score' (acc + 10 + nextRoll) tail            
        | Roll (fst, snd)::tail -> score' (acc + toInt fst + toInt snd) tail
        | [] -> acc
    lastFrameScore + score' 0 frames

let game = Game.create ['X';'X';'X';'X';'X';'X';'X';'X';'X';'X';'X';'X']
let game = Game.create ['9';'-';'9';'-';'9';'-';'9';'-';'9';'-';'9';'-';'9';'-';'9';'-';'9';'-';'9';'-']
let game = Game.create ['5';'/';'5';'/';'5';'/';'5';'/';'5';'/';'5';'/';'5';'/';'5';'/';'5';'/';'5';'/';'5']
let game = Game.create ['X'; 'X'; '3'; '5';'3'; '3']

// Simplest impl https://blogs.msdn.microsoft.com/ashleyf/2010/11/22/bowling-kata/
let rec score' acc = function 
    | 10 :: (a :: b :: _ as t) -> score' (acc + 10 + a + b) t // strike 
    | 10 :: t -> score' acc t // final frames following strike 
    | a :: b :: [c] when a + b = 10 -> acc + 10 + c // final frame spare
    | a :: b :: (c :: _ as t) when a + b = 10 -> score' (acc + 10 + c) t // spare 
    | h :: t -> score' (acc + h) t // normal roll 
    | [] -> acc
