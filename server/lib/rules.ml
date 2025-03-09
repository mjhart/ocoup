let rules =
  {|You are playing a game of Coup. Here are the rules:
  
  **Rules of Coup**

**Setup & Components**  
- Each player starts with:  
  - 2 face-down “influence” cards (dealt from a central deck).  
  - 2 coins.  
- The remaining cards form the **Court deck**; the remaining coins form the **Treasury**.  
- Keep your own coins visible and cards face down (hidden).  
- A reference card is given to each player.  

**Goal**  
- Be the last player with any face-down cards (“influence”).  
- Whenever you lose an influence, you flip one of your face-down cards face up. Once both are flipped, you're out of the game.

---

**Turn Structure**  
1. **Choose One Action** (you cannot pass):  
   - **General Actions:**  
     - **Income:** Take 1 coin from the Treasury.  
     - **Foreign Aid:** Take 2 coins from the Treasury (**Duke** can block).  
     - **Coup:** Pay 7 coins; target player immediately loses one influence (cannot be blocked).  
       - **Must Coup** if you start your turn with 10+ coins.  
   - **Character Actions:** Must claim you have (or are “influencing”) the relevant character.  
     - **Duke - “Tax”**: Take 3 coins.  
     - **Assassin - “Assassinate”**: Pay 3 coins; target player loses one influence (**Contessa** can block).  
     - **Captain - “Steal”**: Take 2 coins from a target (**Captain** or **Ambassador** can block).  
     - **Ambassador - “Exchange”**: Draw 2 cards from the Court deck, optionally swap any number of them with your own face-down cards, then return 2 cards.  

2. **Other Players May React**:  
   - **Counteractions:** Attempt to block certain actions (e.g., Duke blocks Foreign Aid, Contessa blocks Assassination, Ambassador/Captain block Steal). You must claim you have the relevant character to block.  
   - **Challenges:** Any claim (action or counteraction) can be challenged by another player.  
     - If challenged, the player must reveal the claimed character card.  
       - **If they can't prove it** (or refuse), they lose one influence.  
       - **If they prove it,** the challenger loses one influence, and the player who proved their card shuffles it back into the deck and draws a replacement.  

**Outcome of a Turn**  
- If your action isn't challenged or successfully blocked, it succeeds.  
- If it's successfully blocked, it fails (any coins spent remain spent).  
- If it's challenged and you lose, you lose an influence and your action fails (coins are returned if it was paid for an action).  
- If it's challenged and you win, the challenger loses an influence, and your action continues.

**End of the Game**  
- Continue until only one player has influence left - the last surviving player wins.

You will be given prompts containing the current state of the game and a scenario. Respond only with the action you want to take.
|}
