# Comprehensive Test Suite for OCaml Coup Implementation

## Test Organization

Each test case specifies:
- **Starting cards**: [(Player 0 cards), (Player 1 cards)]
- **Move sequence**: List of (player_index, Response) tuples
- **Expected outcome**: What the test validates

## 1. Basic Action Tests

### 1.1 Income Action
**Purpose**: Validate basic income action (no blocks/challenges possible)
```ocaml
Starting cards: [(Duke, Assassin), (Captain, Ambassador)]
Moves:
  (0, Choose_action `Income)
  (1, Choose_action `Income)
Expected: Player 0 has 3 coins, Player 1 has 3 coins
```

### 1.2 Forced Coup at 10+ Coins
**Purpose**: Validate that players with 10+ coins must coup
```ocaml
Starting cards: [(Duke, Duke), (Assassin, Captain)]
Moves:
  (0, Choose_action `Tax)              # +3 coins (5 total)
  (1, Offer_challenge `No_challenge)
  (1, Choose_action `Income)           # +1 coin (3 total)
  (0, Choose_action `Tax)              # +3 coins (8 total)
  (1, Offer_challenge `No_challenge)
  (1, Choose_action `Income)           # +1 coin (4 total)
  (0, Choose_action `Foreign_aid)      # +2 coins (10 total)
  (1, Choose_foreign_aid_response `Allow)
  (1, Choose_action `Income)           # +1 coin (5 total)
  (0, Choose_action (`Coup (Player_id.of_int 1)))  # Must coup with 10+ coins
  (1, Reveal_card `Card_1)
Expected: Player 0 has 3 coins, Player 1 has one influence left
```

## 2. Foreign Aid Tests

### 2.1 Successful Foreign Aid (No Block)
**Purpose**: Validate foreign aid when not blocked
```ocaml
Starting cards: [(Captain, Assassin), (Captain, Ambassador)]
Moves:
  (0, Choose_action `Foreign_aid)
  (1, Choose_foreign_aid_response `Allow)
Expected: Player 0 has 4 coins
```

### 2.2 Duke Blocks Foreign Aid Successfully
**Purpose**: Validate Duke blocking foreign aid
```ocaml
Starting cards: [(Captain, Assassin), (Duke, Ambassador)]
Moves:
  (0, Choose_action `Foreign_aid)
  (1, Choose_foreign_aid_response `Block)
  (0, Offer_challenge `No_challenge)
Expected: Player 0 has 2 coins (no gain)
```

### 2.3 Failed Challenge on Foreign Aid Block
**Purpose**: Player falsely claims Duke to block
```ocaml
Starting cards: [(Captain, Assassin), (Captain, Ambassador)]
Moves:
  (0, Choose_action `Foreign_aid)
  (1, Choose_foreign_aid_response `Block)  # Claims Duke falsely
  (0, Offer_challenge `Challenge)
  (1, Reveal_card `Card_1)  # Reveals Captain, loses influence
Expected: Player 0 has 4 coins, Player 1 loses one influence
```

### 2.4 Successful Challenge on Foreign Aid Block
**Purpose**: Player correctly has Duke when challenged
```ocaml
Starting cards: [(Captain, Assassin), (Duke, Ambassador)]
Moves:
  (0, Choose_action `Foreign_aid)
  (1, Choose_foreign_aid_response `Block)
  (0, Offer_challenge `Challenge)
  (0, Reveal_card `Card_1)  # Challenger loses
Expected: Player 0 loses one influence and has 2 coins, Player 1 gets new card
```

## 3. Tax (Duke) Action Tests

### 3.1 Successful Tax Action
**Purpose**: Validate Duke tax action
```ocaml
Starting cards: [(Duke, Assassin), (Captain, Ambassador)]
Moves:
  (0, Choose_action `Tax)
  (1, Offer_challenge `No_challenge)
Expected: Player 0 has 5 coins
```

### 3.2 Failed Challenge on Tax
**Purpose**: Challenge Tax when player has Duke
```ocaml
Starting cards: [(Duke, Assassin), (Captain, Ambassador)]
Moves:
  (0, Choose_action `Tax)
  (1, Offer_challenge `Challenge)
  (1, Reveal_card `Card_1)  # Challenger loses
Expected: Player 0 has 5 coins and gets new card, Player 1 loses one influence
```

### 3.3 Successful Challenge on Tax
**Purpose**: Challenge Tax when player doesn't have Duke
```ocaml
Starting cards: [(Captain, Assassin), (Duke, Ambassador)]
Moves:
  (0, Choose_action `Tax)
  (1, Offer_challenge `Challenge)
  (0, Reveal_card `Card_1)  # Actor loses
Expected: Player 0 loses one influence and has 2 coins
```

## 4. Assassination Tests

### 4.1 Successful Assassination (No Block/Challenge)
**Purpose**: Basic assassination
```ocaml
Starting cards: [(Duke, Assassin), (Captain, Ambassador)]
Moves:
  (0, Choose_action `Income)
  (1, Choose_action `Income)
  (0, Choose_action (`Assassinate (Player_id.of_int 1)))
  (1, Offer_challenge `No_challenge)
  (1, Choose_assasination_response `Allow)
  (1, Reveal_card `Card_1)
Expected: Player 0 has 0 coins, Player 1 loses one influence
```

### 4.2 Contessa Blocks Assassination
**Purpose**: Successful block with Contessa
```ocaml
Starting cards: [(Duke, Assassin), (Captain, Contessa)]
Moves:
  (0, Choose_action `Income)
  (1, Choose_action `Income)
  (0, Choose_action (`Assassinate (Player_id.of_int 1)))
  (1, Offer_challenge `No_challenge)
  (1, Choose_assasination_response `Block)
  (0, Offer_challenge `No_challenge)
Expected: Player 0 has 0 coins (paid but failed), Player 1 keeps both influences
```

### 4.3 Challenge Assassination - Actor Has Assassin
**Purpose**: Failed challenge on assassination
```ocaml
Starting cards: [(Duke, Assassin), (Captain, Ambassador)]
Moves:
  (0, Choose_action `Income)
  (1, Choose_action `Income)
  (0, Choose_action (`Assassinate (Player_id.of_int 1)))
  (1, Offer_challenge `Challenge)
  (1, Reveal_card `Card_1)  # Challenger loses first influence
  (1, Choose_assasination_response `Allow)
  (1, Reveal_card `Card_2)  # Then loses second from assassination
Expected: Player 0 has 0 coins and gets new card, Player 1 eliminated (0 influences)
```

### 4.4 Challenge Assassination - Actor Doesn't Have Assassin
**Purpose**: Successful challenge on assassination
```ocaml
Starting cards: [(Duke, Captain), (Captain, Ambassador)]
Moves:
  (0, Choose_action `Income)
  (1, Choose_action `Income)
  (0, Choose_action (`Assassinate (Player_id.of_int 1)))
  (1, Offer_challenge `Challenge)
  (0, Reveal_card `Card_1)  # Actor loses
Expected: Player 0 loses one influence and has 3 coins (refunded)
```

### 4.5 Challenge Contessa Block - Has Contessa
**Purpose**: Failed challenge on Contessa block
```ocaml
Starting cards: [(Duke, Assassin), (Captain, Contessa)]
Moves:
  (0, Choose_action `Income)
  (1, Choose_action `Income)
  (0, Choose_action (`Assassinate (Player_id.of_int 1)))
  (1, Offer_challenge `No_challenge)
  (1, Choose_assasination_response `Block)
  (0, Offer_challenge `Challenge)
  (0, Reveal_card `Card_1)  # Challenger loses
Expected: Player 0 loses one influence and has 0 coins, Player 1 gets new card
```

### 4.6 Challenge Contessa Block - Doesn't Have Contessa
**Purpose**: Successful challenge on false Contessa block
```ocaml
Starting cards: [(Duke, Assassin), (Captain, Ambassador)]
Moves:
  (0, Choose_action `Income)
  (1, Choose_action `Income)
  (0, Choose_action (`Assassinate (Player_id.of_int 1)))
  (1, Offer_challenge `No_challenge)
  (1, Choose_assasination_response `Block)  # False claim
  (0, Offer_challenge `Challenge)
  (1, Reveal_card `Card_1)  # Blocker loses first influence
  (1, Reveal_card `Card_2)  # Then loses second from assassination
Expected: Player 0 has 0 coins, Player 1 eliminated
```

## 5. Steal (Captain) Tests

### 5.1 Successful Steal - Target Has 2+ Coins
**Purpose**: Basic steal action
```ocaml
Starting cards: [(Captain, Assassin), (Duke, Ambassador)]
Moves:
  (0, Choose_action (`Steal (Player_id.of_int 1)))
  (1, Offer_challenge `No_challenge)
  (1, Choose_steal_response `Allow)
Expected: Player 0 has 4 coins, Player 1 has 0 coins
```

### 5.2 Steal When Target Has 1 Coin
**Purpose**: Steal limited by target's coins
```ocaml
Starting cards: [(Captain, Assassin), (Duke, Ambassador)]
Moves:
  (0, Choose_action `Income)  # P0: 3 coins
  (1, Choose_action (`Coup (Player_id.of_int 0)))  # P1: 0 coins (spent 7, had 2+5)
  (0, Reveal_card `Card_2)
  (0, Choose_action `Income)  # P0: 4 coins
  (1, Choose_action `Income)  # P1: 1 coin
  (0, Choose_action (`Steal (Player_id.of_int 1)))
  (1, Offer_challenge `No_challenge)
  (1, Choose_steal_response `Allow)
Expected: Player 0 has 5 coins, Player 1 has 0 coins (only 1 stolen)
```

### 5.3 Steal When Target Has 0 Coins
**Purpose**: Steal with no coins available
```ocaml
Starting cards: [(Captain, Assassin), (Duke, Ambassador)]
Moves:
  (0, Choose_action `Tax)  # P0: 5 coins
  (1, Offer_challenge `No_challenge)
  (1, Choose_action `Tax)  # P1: 5 coins
  (0, Offer_challenge `No_challenge)
  (0, Choose_action `Tax)  # P0: 8 coins
  (1, Offer_challenge `No_challenge)
  (1, Choose_action (`Coup (Player_id.of_int 0)))  # P1: 0 coins
  (0, Reveal_card `Card_2)
  (0, Choose_action (`Steal (Player_id.of_int 1)))
  (1, Offer_challenge `No_challenge)
  (1, Choose_steal_response `Allow)
Expected: Player 0 has 8 coins (no gain), Player 1 has 0 coins
```

### 5.4 Captain Blocks Steal
**Purpose**: Captain vs Captain
```ocaml
Starting cards: [(Captain, Assassin), (Captain, Ambassador)]
Moves:
  (0, Choose_action (`Steal (Player_id.of_int 1)))
  (1, Offer_challenge `No_challenge)
  (1, Choose_steal_response (`Block `Captain))
  (0, Offer_challenge `No_challenge)
Expected: Player 0 has 2 coins, Player 1 has 2 coins (steal blocked)
```

### 5.5 Ambassador Blocks Steal
**Purpose**: Ambassador blocking steal
```ocaml
Starting cards: [(Captain, Assassin), (Duke, Ambassador)]
Moves:
  (0, Choose_action (`Steal (Player_id.of_int 1)))
  (1, Offer_challenge `No_challenge)
  (1, Choose_steal_response (`Block `Ambassador))
  (0, Offer_challenge `No_challenge)
Expected: Player 0 has 2 coins, Player 1 has 2 coins (steal blocked)
```

### 5.6 Challenge Steal - Actor Has Captain
**Purpose**: Failed challenge on steal
```ocaml
Starting cards: [(Captain, Assassin), (Duke, Ambassador)]
Moves:
  (0, Choose_action (`Steal (Player_id.of_int 1)))
  (1, Offer_challenge `Challenge)
  (1, Reveal_card `Card_1)  # Challenger loses
Expected: Player 0 has 4 coins and gets new card, Player 1 has 0 coins and loses one influence
```

### 5.7 Challenge Steal - Actor Doesn't Have Captain
**Purpose**: Successful challenge on steal
```ocaml
Starting cards: [(Duke, Assassin), (Captain, Ambassador)]
Moves:
  (0, Choose_action (`Steal (Player_id.of_int 1)))
  (1, Offer_challenge `Challenge)
  (0, Reveal_card `Card_1)  # Actor loses
Expected: Player 0 loses one influence and has 2 coins, Player 1 has 2 coins
```

### 5.8 Challenge Captain Block - Has Captain
**Purpose**: Failed challenge on Captain block
```ocaml
Starting cards: [(Captain, Assassin), (Captain, Duke)]
Moves:
  (0, Choose_action (`Steal (Player_id.of_int 1)))
  (1, Offer_challenge `No_challenge)
  (1, Choose_steal_response (`Block `Captain))
  (0, Offer_challenge `Challenge)
  (0, Reveal_card `Card_1)  # Challenger loses
Expected: Player 0 loses one influence and has 2 coins, Player 1 gets new card and has 2 coins
```

### 5.9 Challenge Ambassador Block - Doesn't Have Ambassador
**Purpose**: Successful challenge on false Ambassador block
```ocaml
Starting cards: [(Captain, Assassin), (Duke, Contessa)]
Moves:
  (0, Choose_action (`Steal (Player_id.of_int 1)))
  (1, Offer_challenge `No_challenge)
  (1, Choose_steal_response (`Block `Ambassador))  # False claim
  (0, Offer_challenge `Challenge)
  (1, Reveal_card `Card_1)  # Blocker loses
Expected: Player 0 has 4 coins, Player 1 has 0 coins and loses one influence
```

## 6. Exchange (Ambassador) Tests

### 6.1 Successful Exchange
**Purpose**: Basic exchange action
```ocaml
Starting cards: [(Duke, Ambassador), (Captain, Assassin)]
Moves:
  (0, Choose_action `Exchange)
  (1, Offer_challenge `No_challenge)
  (0, Choose_cards_to_return)
Expected: Player 0 has different cards (verify deck changed)
```

### 6.2 Challenge Exchange - Has Ambassador
**Purpose**: Failed challenge on exchange
```ocaml
Starting cards: [(Duke, Ambassador), (Captain, Assassin)]
Moves:
  (0, Choose_action `Exchange)
  (1, Offer_challenge `Challenge)
  (1, Reveal_card `Card_1)  # Challenger loses
  (0, Choose_cards_to_return)
Expected: Player 0 gets new card and exchanges, Player 1 loses one influence
```

### 6.3 Challenge Exchange - Doesn't Have Ambassador
**Purpose**: Successful challenge on exchange
```ocaml
Starting cards: [(Duke, Captain), (Captain, Assassin)]
Moves:
  (0, Choose_action `Exchange)
  (1, Offer_challenge `Challenge)
  (0, Reveal_card `Card_1)  # Actor loses
Expected: Player 0 loses one influence, no exchange happens
```

## 7. Coup Tests

### 7.1 Basic Coup
**Purpose**: Standard coup action
```ocaml
Starting cards: [(Duke, Duke), (Captain, Ambassador)]
Moves:
  (0, Choose_action `Tax)  # 5 coins
  (1, Offer_challenge `No_challenge)
  (1, Choose_action `Income)  # 3 coins
  (0, Choose_action `Tax)  # 8 coins
  (1, Offer_challenge `No_challenge)
  (1, Choose_action `Income)  # 4 coins
  (0, Choose_action (`Coup (Player_id.of_int 1)))
  (1, Reveal_card `Card_1)
Expected: Player 0 has 1 coin, Player 1 loses one influence
```

### 7.2 Coup Eliminates Last Influence
**Purpose**: Coup eliminates player
```ocaml
Starting cards: [(Duke, Duke), (Captain, Ambassador)]
Moves:
  (0, Choose_action `Tax)  # 5 coins
  (1, Offer_challenge `No_challenge)
  (1, Choose_action `Income)  # 3 coins
  (0, Choose_action `Tax)  # 8 coins
  (1, Offer_challenge `No_challenge)
  (1, Choose_action `Income)  # 4 coins
  (0, Choose_action (`Coup (Player_id.of_int 1)))
  (1, Reveal_card `Card_1)
  (1, Choose_action `Tax)  # Claims Duke falsely
  (0, Offer_challenge `Challenge)
  (1, Reveal_card `Card_2)  # Loses second influence
Expected: Player 1 eliminated, game ends, Player 0 wins
```

## 8. Complex Interaction Tests

### 8.1 Multiple Challenges in One Game
**Purpose**: Test multiple challenge resolutions
```ocaml
Starting cards: [(Duke, Assassin), (Captain, Ambassador)]
Moves:
  # Round 1: Failed challenge on Tax
  (0, Choose_action `Tax)
  (1, Offer_challenge `Challenge)
  (1, Reveal_card `Card_1)  # P1 loses Captain
  # Round 2: Successful challenge on Steal
  (1, Choose_action (`Steal (Player_id.of_int 0)))
  (0, Offer_challenge `Challenge)
  (1, Reveal_card `Card_2)  # P1 eliminated (had no Captain)
Expected: Player 0 wins with new card from successful defense
```

### 8.2 Bluff Chain - Multiple False Claims
**Purpose**: Test consecutive bluffs
```ocaml
Starting cards: [(Captain, Captain), (Ambassador, Ambassador)]
Moves:
  # P0 falsely claims Duke
  (0, Choose_action `Tax)
  (1, Offer_challenge `No_challenge)  # P1 doesn't challenge
  # P1 falsely claims Assassin
  (1, Choose_action `Income)
  (0, Choose_action `Income)
  (1, Choose_action (`Assassinate (Player_id.of_int 0)))
  (0, Offer_challenge `No_challenge)
  # P0 falsely claims Contessa
  (0, Choose_assasination_response `Block)
  (1, Offer_challenge `No_challenge)  # Both bluffed successfully
Expected: Player 0 has 6 coins, Player 1 has 0 coins, both keep influences
```

### 8.3 Elimination During Multi-Step Action
**Purpose**: Test elimination mid-action
```ocaml
Starting cards: [(Duke, Assassin), (Captain, Ambassador)]
Moves:
  (0, Choose_action `Income)
  (1, Choose_action `Income)
  (0, Choose_action (`Assassinate (Player_id.of_int 1)))
  (1, Offer_challenge `Challenge)  # Challenges assassination
  (1, Reveal_card `Card_1)  # Loses challenge, down to 1 influence
  (1, Choose_assasination_response `Block)  # Tries to block with false Contessa
  (0, Offer_challenge `Challenge)
  (1, Reveal_card `Card_2)  # Loses last influence on false block
Expected: Player 1 eliminated, Player 0 wins with 0 coins
```

### 8.4 All Character Actions in One Game
**Purpose**: Test every character action
```ocaml
Starting cards: [(Duke, Assassin), (Captain, Ambassador)]
Moves:
  # Duke Tax
  (0, Choose_action `Tax)
  (1, Offer_challenge `No_challenge)
  # Ambassador Exchange
  (1, Choose_action `Exchange)
  (0, Offer_challenge `No_challenge)
  (1, Choose_cards_to_return)
  # Assassinate
  (0, Choose_action (`Assassinate (Player_id.of_int 1)))
  (1, Offer_challenge `No_challenge)
  (1, Choose_assasination_response `Allow)
  (1, Reveal_card `Card_1)
  # Captain Steal
  (1, Choose_action (`Steal (Player_id.of_int 0)))
  (0, Offer_challenge `No_challenge)
  (0, Choose_steal_response `Allow)
Expected: All actions executed, P0 has 3 coins, P1 has 2 coins and 1 influence
```

### 8.5 Card Cycling Through Challenges
**Purpose**: Test deck management with multiple card replacements
```ocaml
Starting cards: [(Duke, Duke), (Captain, Captain)]
Moves:
  # P0 gets challenged and wins 3 times
  (0, Choose_action `Tax)
  (1, Offer_challenge `Challenge)
  (1, Reveal_card `Card_1)  # P1 loses, P0 gets new card
  (1, Choose_action (`Steal (Player_id.of_int 0)))
  (0, Offer_challenge `Challenge)
  (0, Reveal_card `Card_1)  # P0 loses, P1 gets new card
  (0, Choose_action `Tax)
  (1, Offer_challenge `No_challenge)
Expected: Both players have shuffled cards from deck
```

## 9. Edge Case Tests

### 9.1 Both Players Lose All Influence Simultaneously
**Purpose**: Test double elimination (should not happen in normal play)
```ocaml
Starting cards: [(Duke, Assassin), (Captain, Ambassador)]
Moves:
  # Get P1 to 1 influence
  (0, Choose_action `Income)
  (1, Choose_action `Income)
  (0, Choose_action (`Assassinate (Player_id.of_int 1)))
  (1, Offer_challenge `No_challenge)
  (1, Choose_assasination_response `Allow)
  (1, Reveal_card `Card_1)
  # P1 false claims assassin
  (1, Choose_action (`Assassinate (Player_id.of_int 0)))
  (0, Offer_challenge `Challenge)
  (1, Reveal_card `Card_2)  # P1 eliminated
Expected: Player 1 eliminated, Player 0 wins
```

### 9.2 Income Only Game
**Purpose**: Test game with only income actions (slowest possible game)
```ocaml
Starting cards: [(Duke, Assassin), (Captain, Ambassador)]
Moves:
  # 8 rounds of income to get to coup
  (0, Choose_action `Income)  # 3
  (1, Choose_action `Income)  # 3
  (0, Choose_action `Income)  # 4
  (1, Choose_action `Income)  # 4
  (0, Choose_action `Income)  # 5
  (1, Choose_action `Income)  # 5
  (0, Choose_action `Income)  # 6
  (1, Choose_action `Income)  # 6
  (0, Choose_action `Income)  # 7
  (1, Choose_action `Income)  # 7
  (0, Choose_action `Income)  # 8
  (1, Choose_action `Income)  # 8
  (0, Choose_action `Income)  # 9
  (1, Choose_action `Income)  # 9
  (0, Choose_action `Income)  # 10
  (1, Choose_action `Income)  # 10
  (0, Choose_action (`Coup (Player_id.of_int 1)))  # Forced
  (1, Reveal_card `Card_1)
Expected: Player 0 has 3 coins, Player 1 has 10 coins with 1 influence
```

### 9.3 Immediate Forced Coup Cascade
**Purpose**: Test forced coups when both players reach 10 coins
```ocaml
Starting cards: [(Duke, Duke), (Duke, Duke)]
Moves:
  # Both players accumulate coins quickly
  (0, Choose_action `Tax)  # 5
  (1, Offer_challenge `No_challenge)
  (1, Choose_action `Tax)  # 5
  (0, Offer_challenge `No_challenge)
  (0, Choose_action `Tax)  # 8
  (1, Offer_challenge `No_challenge)
  (1, Choose_action `Tax)  # 8
  (0, Offer_challenge `No_challenge)
  (0, Choose_action `Foreign_aid)  # 10
  (1, Choose_foreign_aid_response `Allow)
  (1, Choose_action `Foreign_aid)  # 10
  (0, Choose_foreign_aid_response `Allow)
  (0, Choose_action (`Coup (Player_id.of_int 1)))  # Forced
  (1, Reveal_card `Card_1)
  (1, Choose_action (`Coup (Player_id.of_int 0)))  # Forced
  (0, Reveal_card `Card_1)
Expected: Both at 3 coins with 1 influence each
```

### 9.4 Steal From Player With One Influence and No Coins
**Purpose**: Edge case of stealing from weakened player
```ocaml
Starting cards: [(Captain, Duke), (Ambassador, Contessa)]
Moves:
  # Eliminate one of P1's influences
  (0, Choose_action `Tax)  # 5
  (1, Offer_challenge `No_challenge)
  (1, Choose_action `Income)  # 3
  (0, Choose_action `Tax)  # 8
  (1, Offer_challenge `No_challenge)
  (1, Choose_action `Income)  # 4
  (0, Choose_action (`Coup (Player_id.of_int 1)))  # 1
  (1, Reveal_card `Card_1)
  # P1 spends remaining coins
  (1, Choose_action `Income)  # 5
  (0, Choose_action `Income)  # 2
  (1, Choose_action `Income)  # 6
  (0, Choose_action `Income)  # 3
  (1, Choose_action `Income)  # 7
  (0, Choose_action `Income)  # 4
  (1, Choose_action (`Coup (Player_id.of_int 0)))  # 0 coins
  (0, Reveal_card `Card_2)
  # Now steal from P1 who has 1 influence, 0 coins
  (0, Choose_action (`Steal (Player_id.of_int 1)))
  (1, Offer_challenge `No_challenge)
  (1, Choose_steal_response `Allow)
Expected: Player 0 has 4 coins (no gain), Player 1 has 0 coins
```

### 9.5 Maximum Bluff Game
**Purpose**: Players consistently bluff successfully
```ocaml
Starting cards: [(Contessa, Contessa), (Assassin, Assassin)]
Moves:
  # P0 (no Duke) claims Tax repeatedly
  (0, Choose_action `Tax)
  (1, Offer_challenge `No_challenge)
  # P1 (no Captain) claims Steal
  (1, Choose_action (`Steal (Player_id.of_int 0)))
  (0, Offer_challenge `No_challenge)
  (0, Choose_steal_response `Allow)
  # P0 (no Ambassador) claims Exchange
  (0, Choose_action `Exchange)
  (1, Offer_challenge `No_challenge)
  (0, Choose_cards_to_return)
  # P1 (no Duke) blocks foreign aid
  (1, Choose_action `Income)
  (0, Choose_action `Foreign_aid)
  (1, Choose_foreign_aid_response `Block)
  (0, Offer_challenge `No_challenge)
Expected: All bluffs succeed without challenges
```

## 10. Timing and Concurrency Tests

### 10.1 Response Race - First Responder Wins
**Purpose**: Test response race handling
```ocaml
Starting cards: [(Duke, Assassin), (Captain, Ambassador)]
Moves:
  (0, Choose_action `Foreign_aid)
  (1, Choose_foreign_aid_response `Block)  # P1 blocks first (would win race)
  (0, Offer_challenge `No_challenge)
Expected: Player 0 has 2 coins (blocked), Player 1's block succeeds
```

## 11. Invalid Action Tests

### 11.1 Cannot Coup Without Enough Coins
**Purpose**: Verify coup requires 7 coins
```ocaml
Starting cards: [(Duke, Assassin), (Captain, Ambassador)]
Moves:
  (0, Choose_action `Income)  # 3 coins
  (1, Choose_action `Income)
  (0, Choose_action (`Coup (Player_id.of_int 1)))  # Should fail/retry
  # Game should force retry or default to Income
Expected: Action invalid, defaults to Income after retries
```

### 11.2 Cannot Assassinate Without 3 Coins
**Purpose**: Verify assassination cost
```ocaml
Starting cards: [(Duke, Assassin), (Captain, Ambassador)]
Moves:
  (0, Choose_action (`Assassinate (Player_id.of_int 1)))  # Only has 2 coins
  # Should fail and retry
Expected: Action invalid, defaults to Income after retries
```

## 12. Game Termination Tests

### 12.1 Clean Victory Through Coups
**Purpose**: One player systematically eliminates other
```ocaml
Starting cards: [(Duke, Duke), (Captain, Ambassador)]
Moves:
  # P0 accumulates for two coups
  (0, Choose_action `Tax)  # 5
  (1, Offer_challenge `No_challenge)
  (1, Choose_action `Income)  # 3
  (0, Choose_action `Tax)  # 8
  (1, Offer_challenge `No_challenge)
  (1, Choose_action `Income)  # 4
  (0, Choose_action (`Coup (Player_id.of_int 1)))  # -7, P1 loses 1
  (1, Reveal_card `Card_1)
  (1, Choose_action `Income)  # 5
  (0, Choose_action `Tax)  # 4
  (1, Offer_challenge `No_challenge)
  (1, Choose_action `Income)  # 6
  (0, Choose_action `Tax)  # 7
  (1, Offer_challenge `No_challenge)
  (1, Choose_action `Income)  # 7
  (0, Choose_action (`Coup (Player_id.of_int 1)))  # P1 eliminated
  (1, Reveal_card `Card_2)
Expected: Game ends, Player 0 wins
```

### 12.2 Victory Through Successful Challenges
**Purpose**: Win by challenging opponent twice
```ocaml
Starting cards: [(Duke, Assassin), (Contessa, Contessa)]
Moves:
  # P1 false claims twice and gets challenged
  (0, Choose_action `Income)
  (1, Choose_action `Tax)  # False claim (no Duke)
  (0, Offer_challenge `Challenge)
  (1, Reveal_card `Card_1)  # Loses first influence
  (0, Choose_action `Income)
  (1, Choose_action (`Steal (Player_id.of_int 0)))  # False claim (no Captain)
  (0, Offer_challenge `Challenge)
  (1, Reveal_card `Card_2)  # Loses second influence
Expected: Player 1 eliminated, Player 0 wins
```

## Test Execution Notes

1. **Setup**: Each test starts with 2 players, 2 cards each, 2 coins each
2. **Deck**: Contains 11 remaining cards after dealing (3 of each type minus dealt cards)
3. **Validation Points**:
   - Coin counts after each action
   - Influence states (revealed/hidden cards)
   - Deck changes after successful challenge defenses
   - Player elimination and game termination
   - Turn order progression

4. **Key Invariants to Verify**:
   - Total coins in game remains constant (except coup)
   - Total cards (including deck + hands) remains 15
   - Eliminated players removed from game
   - Forced coup at 10+ coins
   - Action costs deducted correctly
   - Successful challenges result in card replacement

5. **Response Types Used**:
   - `Choose_action`: Select action on turn
   - `Choose_assasination_response`: Allow or Block assassination
   - `Choose_foreign_aid_response`: Allow or Block foreign aid
   - `Choose_steal_response`: Allow or Block with Captain/Ambassador
   - `Choose_cards_to_return`: Complete exchange
   - `Reveal_card`: Choose which card to lose
   - `Offer_challenge`: Challenge or No_challenge

This comprehensive test suite covers all game mechanics, edge cases, and complex interactions in the OCaml Coup implementation.