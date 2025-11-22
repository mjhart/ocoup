open! Core
open Async
open Ocoup.For_testing.Types
open Test_helpers

let%expect_test "basic interaction" =
  let%bind () =
    run_test
      ~starting_cards:
        [ (Card.Duke, Card.Assassin); (Card.Captain, Card.Ambassador) ]
      [
        (0, Response.Choose_action `Income);
        (1, Choose_action `Tax);
        (0, Offer_challenge `No_challenge);
        (0, Choose_action `Exchange);
        (0, Choose_cards_to_return);
      ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Duke Assassin)))
       ((id 1) (player_io <opaque>) (coins 2) (hand (Both Captain Ambassador)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador Contessa
       Contessa Contessa)))

    Player 0: Choose_action: Income
    Player 1: Choose_action: Tax
    Player 0: Offer_challenge
    Player 0: Choose_action: Exchange
    Player 0: Choose_cards_to_return

    Game state after all moves:
    ((players
      (((id 1) (player_io <opaque>) (coins 5) (hand (Both Captain Ambassador)))
       ((id 0) (player_io <opaque>) (coins 3) (hand (Both Assassin Duke)))))
     (deck
      (Assassin Contessa Assassin Duke Contessa Ambassador Captain Ambassador
       Contessa Captain Duke)))
    |}];
  return ()

let%expect_test "foreign aid block can be challenged" =
  let%bind () =
    run_test
      ~starting_cards:
        [ (Card.Duke, Card.Assassin); (Card.Captain, Card.Ambassador) ]
      [
        (0, Choose_action `Foreign_aid);
        (1, Choose_foreign_aid_response `Block);
        (0, Offer_challenge `Challenge);
        (1, Reveal_card `Card_1);
      ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Duke Assassin)))
       ((id 1) (player_io <opaque>) (coins 2) (hand (Both Captain Ambassador)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador Contessa
       Contessa Contessa)))

    Player 0: Choose_action: Foreign aid
    Player 1: Choose_foreign_aid_response
    Player 0: Offer_challenge
    Player 1: Reveal_card

    Game state after all moves:
    ((players
      (((id 1) (player_io <opaque>) (coins 2)
        (hand (One (hidden Ambassador) (revealed Captain))))
       ((id 0) (player_io <opaque>) (coins 4) (hand (Both Duke Assassin)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador Contessa
       Contessa Contessa)))
    |}];
  return ()

let%expect_test "contessa block protects against assassination" =
  let%bind () =
    run_test
      ~starting_cards:
        [ (Card.Duke, Card.Assassin); (Card.Captain, Card.Contessa) ]
      [
        (0, Choose_action `Income);
        (1, Choose_action `Income);
        (0, Choose_action (`Assassinate (Player_id.of_int 1)));
        (1, Offer_challenge `No_challenge);
        (1, Choose_assasination_response `Block);
        (0, Offer_challenge `Challenge);
        (0, Reveal_card `Card_2);
      ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Duke Assassin)))
       ((id 1) (player_io <opaque>) (coins 2) (hand (Both Captain Contessa)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador
       Ambassador Contessa Contessa)))

    Player 0: Choose_action: Income
    Player 1: Choose_action: Income
    Player 0: Choose_action: Assassinate 1
    Player 1: Offer_challenge
    Player 1: Choose_assasination_response
    Player 0: Offer_challenge
    Player 0: Reveal_card

    Game state after all moves:
    ((players
      (((id 1) (player_io <opaque>) (coins 3) (hand (Both Captain Assassin)))
       ((id 0) (player_io <opaque>) (coins 0)
        (hand (One (hidden Duke) (revealed Assassin))))))
     (deck
      (Contessa Captain Duke Ambassador Contessa Duke Ambassador Contessa
       Ambassador Captain Assassin)))
    |}];
  return ()

let%expect_test "ambassador block prevents steal" =
  let%bind () =
    run_test
      ~starting_cards:
        [ (Card.Duke, Card.Assassin); (Card.Captain, Card.Ambassador) ]
      [
        (0, Choose_action (`Steal (Player_id.of_int 1)));
        (1, Offer_challenge `No_challenge);
        (1, Choose_steal_response (`Block `Ambassador));
        (0, Offer_challenge `No_challenge);
      ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Duke Assassin)))
       ((id 1) (player_io <opaque>) (coins 2) (hand (Both Captain Ambassador)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador Contessa
       Contessa Contessa)))

    Player 0: Choose_action: Steal 1
    Player 1: Offer_challenge
    Player 1: Choose_steal_response
    Player 0: Offer_challenge

    Game state after all moves:
    ((players
      (((id 1) (player_io <opaque>) (coins 2) (hand (Both Captain Ambassador)))
       ((id 0) (player_io <opaque>) (coins 2) (hand (Both Duke Assassin)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador Contessa
       Contessa Contessa)))
    |}];
  return ()

let%expect_test "failed challenge and assassination cost two influence" =
  let%bind () =
    run_test
      ~starting_cards:
        [ (Card.Duke, Card.Assassin); (Card.Captain, Card.Ambassador) ]
      [
        (0, Choose_action `Income);
        (1, Choose_action `Income);
        (0, Choose_action (`Assassinate (Player_id.of_int 1)));
        (1, Offer_challenge `Challenge);
        (1, Reveal_card `Card_1);
        (1, Choose_assasination_response `Allow);
      ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Duke Assassin)))
       ((id 1) (player_io <opaque>) (coins 2) (hand (Both Captain Ambassador)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador Contessa
       Contessa Contessa)))

    Player 0: Choose_action: Income
    Player 1: Choose_action: Income
    Player 0: Choose_action: Assassinate 1
    Player 1: Offer_challenge
    Player 1: Reveal_card
    Player 1: Choose_assasination_response

    Game state after all moves:
    ((players
      (((id 0) (player_io <opaque>) (coins 0) (hand (Both Duke Assassin)))))
     (deck
      (Contessa Captain Duke Contessa Assassin Duke Ambassador Contessa
       Ambassador Captain Assassin)))
    |}];
  return ()

let%expect_test
    "assassination and successfully challenged block lose two influence" =
  let%bind () =
    run_test
      ~starting_cards:
        [ (Card.Duke, Card.Assassin); (Card.Captain, Card.Ambassador) ]
      [
        (0, Choose_action `Income);
        (1, Choose_action `Income);
        (0, Choose_action (`Assassinate (Player_id.of_int 1)));
        (1, Offer_challenge `No_challenge);
        (1, Choose_assasination_response `Block);
        (0, Offer_challenge `Challenge);
        (1, Reveal_card `Card_1);
      ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Duke Assassin)))
       ((id 1) (player_io <opaque>) (coins 2) (hand (Both Captain Ambassador)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador Contessa
       Contessa Contessa)))

    Player 0: Choose_action: Income
    Player 1: Choose_action: Income
    Player 0: Choose_action: Assassinate 1
    Player 1: Offer_challenge
    Player 1: Choose_assasination_response
    Player 0: Offer_challenge
    Player 1: Reveal_card

    Game state after all moves:
    ((players
      (((id 0) (player_io <opaque>) (coins 0) (hand (Both Duke Assassin)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador Contessa
       Contessa Contessa)))
    |}];
  return ()

(* ========== Basic Action Tests ========== *)

let%expect_test "1.1 income action" =
  let%bind () =
    run_test
      ~starting_cards:
        [ (Card.Duke, Card.Assassin); (Card.Captain, Card.Ambassador) ]
      [ (0, Choose_action `Income); (1, Choose_action `Income) ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Duke Assassin)))
       ((id 1) (player_io <opaque>) (coins 2) (hand (Both Captain Ambassador)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador Contessa
       Contessa Contessa)))

    Player 0: Choose_action: Income
    Player 1: Choose_action: Income

    Game state after all moves:
    ((players
      (((id 0) (player_io <opaque>) (coins 3) (hand (Both Duke Assassin)))
       ((id 1) (player_io <opaque>) (coins 3) (hand (Both Captain Ambassador)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador Contessa
       Contessa Contessa)))
    |}];
  return ()

let%expect_test "1.2 forced coup at 10+ coins" =
  let%bind () =
    run_test
      ~starting_cards:[ (Card.Duke, Card.Duke); (Card.Assassin, Card.Captain) ]
      [
        (0, Choose_action `Tax);
        (1, Offer_challenge `No_challenge);
        (1, Choose_action `Income);
        (0, Choose_action `Tax);
        (1, Offer_challenge `No_challenge);
        (1, Choose_action `Income);
        (0, Choose_action `Foreign_aid);
        (1, Choose_foreign_aid_response `Allow);
        (1, Choose_action `Income);
        (0, Choose_action `Income);
        (0, Choose_action (`Coup (Player_id.of_int 1)));
        (1, Reveal_card `Card_1);
      ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Duke Duke)))
       ((id 1) (player_io <opaque>) (coins 2) (hand (Both Assassin Captain)))))
     (deck
      (Duke Assassin Assassin Captain Captain Ambassador Ambassador Ambassador
       Contessa Contessa Contessa)))

    Player 0: Choose_action: Tax
    Player 1: Offer_challenge
    Player 1: Choose_action: Income
    Player 0: Choose_action: Tax
    Player 1: Offer_challenge
    Player 1: Choose_action: Income
    Player 0: Choose_action: Foreign aid
    Player 1: Choose_foreign_aid_response
    Player 1: Choose_action: Income
    Player 0: Choose_action: Income
    Invalid action
    Player 0: Choose_action: Coup 1
    Player 1: Reveal_card

    Game state after all moves:
    ((players
      (((id 1) (player_io <opaque>) (coins 5)
        (hand (One (hidden Captain) (revealed Assassin))))
       ((id 0) (player_io <opaque>) (coins 3) (hand (Both Duke Duke)))))
     (deck
      (Duke Assassin Assassin Captain Captain Ambassador Ambassador Ambassador
       Contessa Contessa Contessa)))
    |}];
  return ()

(* ========== Foreign Aid Tests ========== *)

let%expect_test "2.1 successful foreign aid (no block)" =
  let%bind () =
    run_test
      ~starting_cards:
        [ (Card.Captain, Card.Assassin); (Card.Captain, Card.Ambassador) ]
      [
        (0, Choose_action `Foreign_aid); (1, Choose_foreign_aid_response `Allow);
      ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Captain Assassin)))
       ((id 1) (player_io <opaque>) (coins 2) (hand (Both Captain Ambassador)))))
     (deck
      (Duke Duke Duke Assassin Assassin Captain Ambassador Ambassador Contessa
       Contessa Contessa)))

    Player 0: Choose_action: Foreign aid
    Player 1: Choose_foreign_aid_response

    Game state after all moves:
    ((players
      (((id 1) (player_io <opaque>) (coins 2) (hand (Both Captain Ambassador)))
       ((id 0) (player_io <opaque>) (coins 4) (hand (Both Captain Assassin)))))
     (deck
      (Duke Duke Duke Assassin Assassin Captain Ambassador Ambassador Contessa
       Contessa Contessa)))
    |}];
  return ()

let%expect_test "2.2 duke blocks foreign aid successfully" =
  let%bind () =
    run_test
      ~starting_cards:
        [ (Card.Captain, Card.Assassin); (Card.Duke, Card.Ambassador) ]
      [
        (0, Choose_action `Foreign_aid);
        (1, Choose_foreign_aid_response `Block);
        (0, Offer_challenge `No_challenge);
      ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Captain Assassin)))
       ((id 1) (player_io <opaque>) (coins 2) (hand (Both Duke Ambassador)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador Contessa
       Contessa Contessa)))

    Player 0: Choose_action: Foreign aid
    Player 1: Choose_foreign_aid_response
    Player 0: Offer_challenge

    Game state after all moves:
    ((players
      (((id 1) (player_io <opaque>) (coins 2) (hand (Both Duke Ambassador)))
       ((id 0) (player_io <opaque>) (coins 2) (hand (Both Captain Assassin)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador Contessa
       Contessa Contessa)))
    |}];
  return ()

let%expect_test "2.4 successful challenge on foreign aid block" =
  let%bind () =
    run_test
      ~starting_cards:
        [ (Card.Captain, Card.Assassin); (Card.Duke, Card.Ambassador) ]
      [
        (0, Choose_action `Foreign_aid);
        (1, Choose_foreign_aid_response `Block);
        (0, Offer_challenge `Challenge);
        (0, Reveal_card `Card_1);
      ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Captain Assassin)))
       ((id 1) (player_io <opaque>) (coins 2) (hand (Both Duke Ambassador)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador Contessa
       Contessa Contessa)))

    Player 0: Choose_action: Foreign aid
    Player 1: Choose_foreign_aid_response
    Player 0: Offer_challenge
    Player 0: Reveal_card

    Game state after all moves:
    ((players
      (((id 1) (player_io <opaque>) (coins 2) (hand (Both Assassin Ambassador)))
       ((id 0) (player_io <opaque>) (coins 2)
        (hand (One (hidden Assassin) (revealed Captain))))))
     (deck
      (Contessa Captain Duke Contessa Duke Duke Ambassador Contessa Ambassador
       Captain Assassin)))
    |}];
  return ()

(* ========== Tax (Duke) Action Tests ========== *)

let%expect_test "3.1 successful tax action" =
  let%bind () =
    run_test
      ~starting_cards:
        [ (Card.Duke, Card.Assassin); (Card.Captain, Card.Ambassador) ]
      [ (0, Choose_action `Tax); (1, Offer_challenge `No_challenge) ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Duke Assassin)))
       ((id 1) (player_io <opaque>) (coins 2) (hand (Both Captain Ambassador)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador Contessa
       Contessa Contessa)))

    Player 0: Choose_action: Tax
    Player 1: Offer_challenge

    Game state after all moves:
    ((players
      (((id 1) (player_io <opaque>) (coins 2) (hand (Both Captain Ambassador)))
       ((id 0) (player_io <opaque>) (coins 5) (hand (Both Duke Assassin)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador Contessa
       Contessa Contessa)))
    |}];
  return ()

let%expect_test "3.2 failed challenge on tax" =
  let%bind () =
    run_test
      ~starting_cards:
        [ (Card.Duke, Card.Assassin); (Card.Captain, Card.Ambassador) ]
      [
        (0, Choose_action `Tax);
        (1, Offer_challenge `Challenge);
        (1, Reveal_card `Card_1);
      ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Duke Assassin)))
       ((id 1) (player_io <opaque>) (coins 2) (hand (Both Captain Ambassador)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador Contessa
       Contessa Contessa)))

    Player 0: Choose_action: Tax
    Player 1: Offer_challenge
    Player 1: Reveal_card

    Game state after all moves:
    ((players
      (((id 1) (player_io <opaque>) (coins 2)
        (hand (One (hidden Ambassador) (revealed Captain))))
       ((id 0) (player_io <opaque>) (coins 5) (hand (Both Assassin Assassin)))))
     (deck
      (Contessa Captain Duke Contessa Duke Duke Ambassador Contessa Ambassador
       Captain Assassin)))
    |}];
  return ()

let%expect_test "3.3 successful challenge on tax" =
  let%bind () =
    run_test
      ~starting_cards:
        [ (Card.Captain, Card.Assassin); (Card.Duke, Card.Ambassador) ]
      [
        (0, Choose_action `Tax);
        (1, Offer_challenge `Challenge);
        (0, Reveal_card `Card_1);
      ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Captain Assassin)))
       ((id 1) (player_io <opaque>) (coins 2) (hand (Both Duke Ambassador)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador Contessa
       Contessa Contessa)))

    Player 0: Choose_action: Tax
    Player 1: Offer_challenge
    Player 0: Reveal_card

    Game state after all moves:
    ((players
      (((id 1) (player_io <opaque>) (coins 2) (hand (Both Duke Ambassador)))
       ((id 0) (player_io <opaque>) (coins 2)
        (hand (One (hidden Assassin) (revealed Captain))))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador Contessa
       Contessa Contessa)))
    |}];
  return ()

(* ========== Assassination Tests ========== *)

let%expect_test "4.1 successful assassination (no block/challenge)" =
  let%bind () =
    run_test
      ~starting_cards:
        [ (Card.Duke, Card.Assassin); (Card.Captain, Card.Ambassador) ]
      [
        (0, Choose_action `Income);
        (1, Choose_action `Income);
        (0, Choose_action (`Assassinate (Player_id.of_int 1)));
        (1, Offer_challenge `No_challenge);
        (1, Choose_assasination_response `Allow);
        (1, Reveal_card `Card_1);
      ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Duke Assassin)))
       ((id 1) (player_io <opaque>) (coins 2) (hand (Both Captain Ambassador)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador Contessa
       Contessa Contessa)))

    Player 0: Choose_action: Income
    Player 1: Choose_action: Income
    Player 0: Choose_action: Assassinate 1
    Player 1: Offer_challenge
    Player 1: Choose_assasination_response
    Player 1: Reveal_card

    Game state after all moves:
    ((players
      (((id 1) (player_io <opaque>) (coins 3)
        (hand (One (hidden Ambassador) (revealed Captain))))
       ((id 0) (player_io <opaque>) (coins 0) (hand (Both Duke Assassin)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador Contessa
       Contessa Contessa)))
    |}];
  return ()

let%expect_test "4.4 challenge assassination - actor doesn't have assassin" =
  let%bind () =
    run_test
      ~starting_cards:
        [ (Card.Duke, Card.Captain); (Card.Captain, Card.Ambassador) ]
      [
        (0, Choose_action `Income);
        (1, Choose_action `Income);
        (0, Choose_action (`Assassinate (Player_id.of_int 1)));
        (1, Offer_challenge `Challenge);
        (0, Reveal_card `Card_1);
      ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Duke Captain)))
       ((id 1) (player_io <opaque>) (coins 2) (hand (Both Captain Ambassador)))))
     (deck
      (Duke Duke Assassin Assassin Assassin Captain Ambassador Ambassador
       Contessa Contessa Contessa)))

    Player 0: Choose_action: Income
    Player 1: Choose_action: Income
    Player 0: Choose_action: Assassinate 1
    Player 1: Offer_challenge
    Player 0: Reveal_card

    Game state after all moves:
    ((players
      (((id 1) (player_io <opaque>) (coins 3) (hand (Both Captain Ambassador)))
       ((id 0) (player_io <opaque>) (coins 0)
        (hand (One (hidden Captain) (revealed Duke))))))
     (deck
      (Duke Duke Assassin Assassin Assassin Captain Ambassador Ambassador
       Contessa Contessa Contessa)))
    |}];
  return ()

let%expect_test "4.5 challenge contessa block - has contessa" =
  let%bind () =
    run_test
      ~starting_cards:
        [ (Card.Duke, Card.Assassin); (Card.Captain, Card.Contessa) ]
      [
        (0, Choose_action `Income);
        (1, Choose_action `Income);
        (0, Choose_action (`Assassinate (Player_id.of_int 1)));
        (1, Offer_challenge `No_challenge);
        (1, Choose_assasination_response `Block);
        (0, Offer_challenge `Challenge);
        (0, Reveal_card `Card_1);
      ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Duke Assassin)))
       ((id 1) (player_io <opaque>) (coins 2) (hand (Both Captain Contessa)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador
       Ambassador Contessa Contessa)))

    Player 0: Choose_action: Income
    Player 1: Choose_action: Income
    Player 0: Choose_action: Assassinate 1
    Player 1: Offer_challenge
    Player 1: Choose_assasination_response
    Player 0: Offer_challenge
    Player 0: Reveal_card

    Game state after all moves:
    ((players
      (((id 1) (player_io <opaque>) (coins 3) (hand (Both Captain Assassin)))
       ((id 0) (player_io <opaque>) (coins 0)
        (hand (One (hidden Assassin) (revealed Duke))))))
     (deck
      (Contessa Captain Duke Ambassador Contessa Duke Ambassador Contessa
       Ambassador Captain Assassin)))
    |}];
  return ()

(* ========== Steal (Captain) Tests ========== *)

let%expect_test "5.1 successful steal - target has 2+ coins" =
  let%bind () =
    run_test
      ~starting_cards:
        [ (Card.Captain, Card.Assassin); (Card.Duke, Card.Ambassador) ]
      [
        (0, Choose_action (`Steal (Player_id.of_int 1)));
        (1, Offer_challenge `No_challenge);
        (1, Choose_steal_response `Allow);
      ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Captain Assassin)))
       ((id 1) (player_io <opaque>) (coins 2) (hand (Both Duke Ambassador)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador Contessa
       Contessa Contessa)))

    Player 0: Choose_action: Steal 1
    Player 1: Offer_challenge
    Player 1: Choose_steal_response

    Game state after all moves:
    ((players
      (((id 1) (player_io <opaque>) (coins 0) (hand (Both Duke Ambassador)))
       ((id 0) (player_io <opaque>) (coins 4) (hand (Both Captain Assassin)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador Contessa
       Contessa Contessa)))
    |}];
  return ()

let%expect_test "5.4 captain blocks steal" =
  let%bind () =
    run_test
      ~starting_cards:
        [ (Card.Captain, Card.Assassin); (Card.Captain, Card.Ambassador) ]
      [
        (0, Choose_action (`Steal (Player_id.of_int 1)));
        (1, Offer_challenge `No_challenge);
        (1, Choose_steal_response (`Block `Captain));
        (0, Offer_challenge `No_challenge);
      ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Captain Assassin)))
       ((id 1) (player_io <opaque>) (coins 2) (hand (Both Captain Ambassador)))))
     (deck
      (Duke Duke Duke Assassin Assassin Captain Ambassador Ambassador Contessa
       Contessa Contessa)))

    Player 0: Choose_action: Steal 1
    Player 1: Offer_challenge
    Player 1: Choose_steal_response
    Player 0: Offer_challenge

    Game state after all moves:
    ((players
      (((id 1) (player_io <opaque>) (coins 2) (hand (Both Captain Ambassador)))
       ((id 0) (player_io <opaque>) (coins 2) (hand (Both Captain Assassin)))))
     (deck
      (Duke Duke Duke Assassin Assassin Captain Ambassador Ambassador Contessa
       Contessa Contessa)))
    |}];
  return ()

let%expect_test "5.7 challenge steal - actor doesn't have captain" =
  let%bind () =
    run_test
      ~starting_cards:
        [ (Card.Duke, Card.Assassin); (Card.Captain, Card.Ambassador) ]
      [
        (0, Choose_action (`Steal (Player_id.of_int 1)));
        (1, Offer_challenge `Challenge);
        (0, Reveal_card `Card_1);
      ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Duke Assassin)))
       ((id 1) (player_io <opaque>) (coins 2) (hand (Both Captain Ambassador)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador Contessa
       Contessa Contessa)))

    Player 0: Choose_action: Steal 1
    Player 1: Offer_challenge
    Player 0: Reveal_card

    Game state after all moves:
    ((players
      (((id 1) (player_io <opaque>) (coins 2) (hand (Both Captain Ambassador)))
       ((id 0) (player_io <opaque>) (coins 2)
        (hand (One (hidden Assassin) (revealed Duke))))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador Contessa
       Contessa Contessa)))
    |}];
  return ()

let%expect_test "5.8 challenge captain block - has captain" =
  let%bind () =
    run_test
      ~starting_cards:
        [ (Card.Captain, Card.Assassin); (Card.Captain, Card.Duke) ]
      [
        (0, Choose_action (`Steal (Player_id.of_int 1)));
        (1, Offer_challenge `No_challenge);
        (1, Choose_steal_response (`Block `Captain));
        (0, Offer_challenge `Challenge);
        (0, Reveal_card `Card_1);
      ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Captain Assassin)))
       ((id 1) (player_io <opaque>) (coins 2) (hand (Both Captain Duke)))))
     (deck
      (Duke Duke Assassin Assassin Captain Ambassador Ambassador Ambassador
       Contessa Contessa Contessa)))

    Player 0: Choose_action: Steal 1
    Player 1: Offer_challenge
    Player 1: Choose_steal_response
    Player 0: Offer_challenge
    Player 0: Reveal_card

    Game state after all moves:
    ((players
      (((id 1) (player_io <opaque>) (coins 2) (hand (Both Assassin Duke)))
       ((id 0) (player_io <opaque>) (coins 2)
        (hand (One (hidden Assassin) (revealed Captain))))))
     (deck
      (Contessa Ambassador Duke Contessa Captain Duke Ambassador Contessa
       Ambassador Captain Assassin)))
    |}];
  return ()

let%expect_test "5.9 challenge ambassador block - doesn't have ambassador" =
  let%bind () =
    run_test
      ~starting_cards:
        [ (Card.Captain, Card.Assassin); (Card.Duke, Card.Contessa) ]
      [
        (0, Choose_action (`Steal (Player_id.of_int 1)));
        (1, Offer_challenge `No_challenge);
        (1, Choose_steal_response (`Block `Ambassador));
        (0, Offer_challenge `Challenge);
        (1, Reveal_card `Card_1);
      ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Captain Assassin)))
       ((id 1) (player_io <opaque>) (coins 2) (hand (Both Duke Contessa)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador
       Ambassador Contessa Contessa)))

    Player 0: Choose_action: Steal 1
    Player 1: Offer_challenge
    Player 1: Choose_steal_response
    Player 0: Offer_challenge
    Player 1: Reveal_card

    Game state after all moves:
    ((players
      (((id 1) (player_io <opaque>) (coins 0)
        (hand (One (hidden Contessa) (revealed Duke))))
       ((id 0) (player_io <opaque>) (coins 4) (hand (Both Captain Assassin)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador
       Ambassador Contessa Contessa)))
    |}];
  return ()

(* ========== Exchange (Ambassador) Tests ========== *)

(* ========== Coup Tests ========== *)

let%expect_test "7.1 basic coup" =
  let%bind () =
    run_test
      ~starting_cards:
        [ (Card.Duke, Card.Duke); (Card.Captain, Card.Ambassador) ]
      [
        (0, Choose_action `Tax);
        (1, Offer_challenge `No_challenge);
        (1, Choose_action `Income);
        (0, Choose_action `Tax);
        (1, Offer_challenge `No_challenge);
        (1, Choose_action `Income);
        (0, Choose_action (`Coup (Player_id.of_int 1)));
        (1, Reveal_card `Card_1);
      ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Duke Duke)))
       ((id 1) (player_io <opaque>) (coins 2) (hand (Both Captain Ambassador)))))
     (deck
      (Duke Assassin Assassin Assassin Captain Captain Ambassador Ambassador
       Contessa Contessa Contessa)))

    Player 0: Choose_action: Tax
    Player 1: Offer_challenge
    Player 1: Choose_action: Income
    Player 0: Choose_action: Tax
    Player 1: Offer_challenge
    Player 1: Choose_action: Income
    Player 0: Choose_action: Coup 1
    Player 1: Reveal_card

    Game state after all moves:
    ((players
      (((id 1) (player_io <opaque>) (coins 4)
        (hand (One (hidden Ambassador) (revealed Captain))))
       ((id 0) (player_io <opaque>) (coins 1) (hand (Both Duke Duke)))))
     (deck
      (Duke Assassin Assassin Assassin Captain Captain Ambassador Ambassador
       Contessa Contessa Contessa)))
    |}];
  return ()

let%expect_test "7.2 coup eliminates last influence" =
  let%bind () =
    run_test
      ~starting_cards:
        [ (Card.Duke, Card.Duke); (Card.Captain, Card.Ambassador) ]
      [
        (0, Choose_action `Tax);
        (1, Offer_challenge `No_challenge);
        (1, Choose_action `Income);
        (0, Choose_action `Tax);
        (1, Offer_challenge `No_challenge);
        (1, Choose_action `Income);
        (0, Choose_action (`Coup (Player_id.of_int 1)));
        (1, Reveal_card `Card_1);
        (1, Choose_action `Tax);
        (0, Offer_challenge `Challenge);
        (1, Reveal_card `Card_2);
      ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Duke Duke)))
       ((id 1) (player_io <opaque>) (coins 2) (hand (Both Captain Ambassador)))))
     (deck
      (Duke Assassin Assassin Assassin Captain Captain Ambassador Ambassador
       Contessa Contessa Contessa)))

    Player 0: Choose_action: Tax
    Player 1: Offer_challenge
    Player 1: Choose_action: Income
    Player 0: Choose_action: Tax
    Player 1: Offer_challenge
    Player 1: Choose_action: Income
    Player 0: Choose_action: Coup 1
    Player 1: Reveal_card
    Player 1: Choose_action: Tax
    Player 0: Offer_challenge

    Game state after all moves:
    ((players (((id 0) (player_io <opaque>) (coins 1) (hand (Both Duke Duke)))))
     (deck
      (Duke Assassin Assassin Assassin Captain Captain Ambassador Ambassador
       Contessa Contessa Contessa)))
    |}];
  return ()

(* ========== Complex Interaction Tests ========== *)

let%expect_test "8.1 multiple challenges in one game" =
  let%bind () =
    run_test
      ~starting_cards:
        [ (Card.Duke, Card.Assassin); (Card.Captain, Card.Ambassador) ]
      [
        (0, Choose_action `Tax);
        (1, Offer_challenge `Challenge);
        (1, Reveal_card `Card_1);
        (1, Choose_action (`Steal (Player_id.of_int 0)));
        (0, Offer_challenge `Challenge);
        (1, Reveal_card `Card_2);
      ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Duke Assassin)))
       ((id 1) (player_io <opaque>) (coins 2) (hand (Both Captain Ambassador)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador Contessa
       Contessa Contessa)))

    Player 0: Choose_action: Tax
    Player 1: Offer_challenge
    Player 1: Reveal_card
    Player 1: Choose_action: Steal 0
    Player 0: Offer_challenge

    Game state after all moves:
    ((players
      (((id 0) (player_io <opaque>) (coins 5) (hand (Both Assassin Assassin)))))
     (deck
      (Contessa Captain Duke Contessa Duke Duke Ambassador Contessa Ambassador
       Captain Assassin)))
    |}];
  return ()

let%expect_test "8.2 bluff chain - multiple false claims" =
  let%bind () =
    run_test
      ~starting_cards:
        [ (Card.Captain, Card.Captain); (Card.Ambassador, Card.Ambassador) ]
      [
        (0, Choose_action `Tax);
        (1, Offer_challenge `No_challenge);
        (1, Choose_action `Income);
        (0, Choose_action `Income);
        (1, Choose_action (`Assassinate (Player_id.of_int 0)));
        (0, Offer_challenge `No_challenge);
        (0, Choose_assasination_response `Block);
        (1, Offer_challenge `No_challenge);
      ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Captain Captain)))
       ((id 1) (player_io <opaque>) (coins 2)
        (hand (Both Ambassador Ambassador)))))
     (deck
      (Duke Duke Duke Assassin Assassin Assassin Captain Ambassador Contessa
       Contessa Contessa)))

    Player 0: Choose_action: Tax
    Player 1: Offer_challenge
    Player 1: Choose_action: Income
    Player 0: Choose_action: Income
    Player 1: Choose_action: Assassinate 0
    Player 0: Offer_challenge
    Player 0: Choose_assasination_response
    Player 1: Offer_challenge

    Game state after all moves:
    ((players
      (((id 0) (player_io <opaque>) (coins 6) (hand (Both Captain Captain)))
       ((id 1) (player_io <opaque>) (coins 0)
        (hand (Both Ambassador Ambassador)))))
     (deck
      (Duke Duke Duke Assassin Assassin Assassin Captain Ambassador Contessa
       Contessa Contessa)))
    |}];
  return ()

(* ========== Edge Case Tests ========== *)

let%expect_test "9.2 income only game" =
  let%bind () =
    run_test
      ~starting_cards:
        [ (Card.Duke, Card.Assassin); (Card.Captain, Card.Ambassador) ]
      [
        (0, Choose_action `Income);
        (1, Choose_action `Income);
        (0, Choose_action `Income);
        (1, Choose_action `Income);
        (0, Choose_action `Income);
        (1, Choose_action `Income);
        (0, Choose_action `Income);
        (1, Choose_action `Income);
        (0, Choose_action `Income);
        (1, Choose_action `Income);
        (0, Choose_action `Income);
        (1, Choose_action `Income);
        (0, Choose_action `Income);
        (1, Choose_action `Income);
        (0, Choose_action `Income);
        (1, Choose_action `Income);
        (0, Choose_action (`Coup (Player_id.of_int 1)));
        (1, Reveal_card `Card_1);
      ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Duke Assassin)))
       ((id 1) (player_io <opaque>) (coins 2) (hand (Both Captain Ambassador)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador Contessa
       Contessa Contessa)))

    Player 0: Choose_action: Income
    Player 1: Choose_action: Income
    Player 0: Choose_action: Income
    Player 1: Choose_action: Income
    Player 0: Choose_action: Income
    Player 1: Choose_action: Income
    Player 0: Choose_action: Income
    Player 1: Choose_action: Income
    Player 0: Choose_action: Income
    Player 1: Choose_action: Income
    Player 0: Choose_action: Income
    Player 1: Choose_action: Income
    Player 0: Choose_action: Income
    Player 1: Choose_action: Income
    Player 0: Choose_action: Income
    Player 1: Choose_action: Income
    Player 0: Choose_action: Coup 1
    Player 1: Reveal_card

    Game state after all moves:
    ((players
      (((id 1) (player_io <opaque>) (coins 10)
        (hand (One (hidden Ambassador) (revealed Captain))))
       ((id 0) (player_io <opaque>) (coins 3) (hand (Both Duke Assassin)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Ambassador Ambassador Contessa
       Contessa Contessa)))
    |}];
  return ()

let%expect_test "9.3 immediate forced coup cascade" =
  let%bind () =
    run_test
      ~starting_cards:[ (Card.Duke, Card.Duke); (Card.Duke, Card.Captain) ]
      [
        (0, Choose_action `Tax);
        (1, Offer_challenge `No_challenge);
        (1, Choose_action `Tax);
        (0, Offer_challenge `No_challenge);
        (0, Choose_action `Tax);
        (1, Offer_challenge `No_challenge);
        (1, Choose_action `Tax);
        (0, Offer_challenge `No_challenge);
        (0, Choose_action `Foreign_aid);
        (1, Choose_foreign_aid_response `Allow);
        (1, Choose_action `Foreign_aid);
        (0, Choose_foreign_aid_response `Allow);
        (0, Choose_action (`Coup (Player_id.of_int 1)));
        (1, Reveal_card `Card_1);
        (1, Choose_action (`Coup (Player_id.of_int 0)));
        (0, Reveal_card `Card_1);
      ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Duke Duke)))
       ((id 1) (player_io <opaque>) (coins 2) (hand (Both Duke Captain)))))
     (deck
      (Assassin Assassin Assassin Captain Captain Ambassador Ambassador
       Ambassador Contessa Contessa Contessa)))

    Player 0: Choose_action: Tax
    Player 1: Offer_challenge
    Player 1: Choose_action: Tax
    Player 0: Offer_challenge
    Player 0: Choose_action: Tax
    Player 1: Offer_challenge
    Player 1: Choose_action: Tax
    Player 0: Offer_challenge
    Player 0: Choose_action: Foreign aid
    Player 1: Choose_foreign_aid_response
    Player 1: Choose_action: Foreign aid
    Player 0: Choose_foreign_aid_response
    Player 0: Choose_action: Coup 1
    Player 1: Reveal_card
    Player 1: Choose_action: Coup 0
    Player 0: Reveal_card

    Game state after all moves:
    ((players
      (((id 0) (player_io <opaque>) (coins 3)
        (hand (One (hidden Duke) (revealed Duke))))
       ((id 1) (player_io <opaque>) (coins 3)
        (hand (One (hidden Captain) (revealed Duke))))))
     (deck
      (Assassin Assassin Assassin Captain Captain Ambassador Ambassador
       Ambassador Contessa Contessa Contessa)))
    |}];
  return ()

(* ========== Game Termination Tests ========== *)

let%expect_test "12.1 clean victory through coups" =
  let%bind () =
    run_test
      ~starting_cards:
        [ (Card.Duke, Card.Duke); (Card.Captain, Card.Ambassador) ]
      [
        (0, Choose_action `Tax);
        (1, Offer_challenge `No_challenge);
        (1, Choose_action `Income);
        (0, Choose_action `Tax);
        (1, Offer_challenge `No_challenge);
        (1, Choose_action `Income);
        (0, Choose_action (`Coup (Player_id.of_int 1)));
        (1, Reveal_card `Card_1);
        (1, Choose_action `Income);
        (0, Choose_action `Tax);
        (1, Offer_challenge `No_challenge);
        (1, Choose_action `Income);
        (0, Choose_action `Tax);
        (1, Offer_challenge `No_challenge);
        (1, Choose_action `Income);
        (0, Choose_action (`Coup (Player_id.of_int 1)));
        (1, Reveal_card `Card_2);
      ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Duke Duke)))
       ((id 1) (player_io <opaque>) (coins 2) (hand (Both Captain Ambassador)))))
     (deck
      (Duke Assassin Assassin Assassin Captain Captain Ambassador Ambassador
       Contessa Contessa Contessa)))

    Player 0: Choose_action: Tax
    Player 1: Offer_challenge
    Player 1: Choose_action: Income
    Player 0: Choose_action: Tax
    Player 1: Offer_challenge
    Player 1: Choose_action: Income
    Player 0: Choose_action: Coup 1
    Player 1: Reveal_card
    Player 1: Choose_action: Income
    Player 0: Choose_action: Tax
    Player 1: Offer_challenge
    Player 1: Choose_action: Income
    Player 0: Choose_action: Tax
    Player 1: Offer_challenge
    Player 1: Choose_action: Income
    Player 0: Choose_action: Coup 1

    Game state after all moves:
    ((players (((id 0) (player_io <opaque>) (coins 0) (hand (Both Duke Duke)))))
     (deck
      (Duke Assassin Assassin Assassin Captain Captain Ambassador Ambassador
       Contessa Contessa Contessa)))
    |}];
  return ()

let%expect_test "12.2 victory through successful challenges" =
  let%bind () =
    run_test
      ~starting_cards:
        [ (Card.Duke, Card.Assassin); (Card.Contessa, Card.Contessa) ]
      [
        (0, Choose_action `Income);
        (1, Choose_action `Tax);
        (0, Offer_challenge `Challenge);
        (1, Reveal_card `Card_1);
        (0, Choose_action `Income);
        (1, Choose_action (`Steal (Player_id.of_int 0)));
        (0, Offer_challenge `Challenge);
        (1, Reveal_card `Card_2);
      ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Duke Assassin)))
       ((id 1) (player_io <opaque>) (coins 2) (hand (Both Contessa Contessa)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Captain Ambassador Ambassador
       Ambassador Contessa)))

    Player 0: Choose_action: Income
    Player 1: Choose_action: Tax
    Player 0: Offer_challenge
    Player 1: Reveal_card
    Player 0: Choose_action: Income
    Player 1: Choose_action: Steal 0
    Player 0: Offer_challenge

    Game state after all moves:
    ((players
      (((id 0) (player_io <opaque>) (coins 4) (hand (Both Duke Assassin)))))
     (deck
      (Duke Duke Assassin Assassin Captain Captain Captain Ambassador Ambassador
       Ambassador Contessa)))
    |}];
  return ()

let%expect_test "notifications" =
  let%bind () =
    run_test ~print_notifications:()
      ~starting_cards:
        [
          (Card.Duke, Card.Assassin);
          (Card.Contessa, Card.Contessa);
          (Card.Assassin, Card.Captain);
        ]
      [
        (0, Choose_action `Tax);
        (1, Offer_challenge `Challenge);
        (2, Offer_challenge `No_challenge);
        (1, Reveal_card `Card_1);
      ]
  in
  [%expect
    {|
    Initial game state:
    ((players
      (((id 0) (player_io <opaque>) (coins 2) (hand (Both Duke Assassin)))
       ((id 1) (player_io <opaque>) (coins 2) (hand (Both Contessa Contessa)))
       ((id 2) (player_io <opaque>) (coins 2) (hand (Both Assassin Captain)))))
     (deck
      (Duke Duke Assassin Captain Captain Ambassador Ambassador Ambassador
       Contessa)))

    Player 0: Choose_action: Tax
    Player 1: Offer_challenge
    Player 2: Offer_challenge
    Player 0: Player 1 challenged (has_required_card: true)
    Player 1: Player 1 challenged (has_required_card: true)
    Player 2: Player 1 challenged (has_required_card: true)
    Player 0: Received new card (Duke)
    Player 1: Reveal_card
    Player 0: Player 1 lost influence (Contessa)
    Player 1: Player 1 lost influence (Contessa)
    Player 2: Player 1 lost influence (Contessa)

    Game state after all moves:
    ((players
      (((id 1) (player_io <opaque>) (coins 2)
        (hand (One (hidden Contessa) (revealed Contessa))))
       ((id 2) (player_io <opaque>) (coins 2) (hand (Both Assassin Captain)))
       ((id 0) (player_io <opaque>) (coins 5) (hand (Both Duke Assassin)))))
     (deck
      (Duke Assassin Captain Ambassador Ambassador Ambassador Captain Duke
       Contessa)))
    |}];
  return ()
