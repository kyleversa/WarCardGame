defmodule War do
  @moduledoc """
    Documentation for `War`.
  """

  @doc """
    Function stub for deal/1 is given below. Feel free to add
    as many additional helper functions as you want.

    The tests for the deal function can be found in test/war_test.exs.
    You can add your five test cases to this file.

    Run the tester by executing 'mix test' from the war directory
    (the one containing mix.exs)
  """

  def deal(shuf) do
    shuf = Enum.reverse(shuf)
    {player1, player2} = distribute_cards(shuf, [], [], 0)
    winner = play_recursive(player1, player2)
    winner = Enum.map(winner, fn x -> if x == 14, do: 1, else: x end)
    winner
  end

  defp distribute_cards(shuf, player1, player2, cursor) do
    if cursor == length(shuf) do
      {player1, player2}
    else
      {card, _} = List.pop_at(shuf, cursor)
      cursor = cursor + 1
      remainder = rem(cursor, 2)

      card = remap_ace(card)
      player1 = if remainder == 1, do: player1 ++ [card], else: player1
      player2 = if remainder == 0, do: player2 ++ [card], else: player2
      distribute_cards(shuf, player1, player2, cursor)
    end
  end

  defp play_recursive(player1, player2) do
    # Base case is if player1 or player2 is empty
    if Enum.empty?(player1) or Enum.empty?(player2) do
      winner = if Enum.empty?(player1), do: player2, else:
        player1
      winner
    else
      {card1, card2, player1, player2} = deal_two_cards(player1, player2)
      # If cards are equal, go to war
      {player1, player2} = if card1 == card2, do:
        fight_war(player1, player2, [card1, card2]), else: {player1, player2}
      player1 = if card1 > card2, do: player1 ++ [card1, card2], else: player1
      player2 = if card2 > card1, do: player2 ++ [card2, card1], else: player2

      # Call itself until the base case is achieved
      play_recursive(player1, player2)
    end
  end

  defp deal_two_cards(player1, player2) do
    # Distribute cards from the top of each player's deck.
    {card1, player1} = distribute_one_card(player1)
    {card2, player2} = distribute_one_card(player2)
    {card1, card2, player1, player2}
  end

  defp distribute_one_card(deck) do
    List.pop_at(deck, 0)
  end

  defp fight_war(player1, player2, war_cards) do

    # Recurses until one player has no cards left or cards are different.
    {card1, _} = List.pop_at(war_cards, -2)
    {card2, _} = List.pop_at(war_cards, -1)

    if Enum.empty?(player1) or Enum.empty?(player2) or card1 != card2 do
      # If one player does not have any card and the latest cards are equal,
      # add the warcards to the other player
      war_cards = Enum.sort(war_cards, :desc)
      player2 = if Enum.empty?(player1) and card1 == card2, do: player2 ++ war_cards, else: player2
      player1 = if Enum.empty?(player2) and card2 == card1, do: player1 ++ war_cards, else: player1
      # If player's card is greater than the other, award them the warcards
      player1 = if card1 > card2, do: player1 ++ war_cards, else: player1
      player2 = if card2 > card1, do: player2 ++ war_cards, else: player2
      {player1, player2}
    else
      {face_down1, face_down2, player1, player2} = deal_two_cards(player1, player2)

      if Enum.empty?(player1) or Enum.empty?(player2) do
        # If any player runs out of cards after the first ones have been drawn
        # add warcards to the other and return their piles
        war_cards = war_cards ++ [face_down1, face_down2]
        war_cards = Enum.sort(war_cards, :desc)
        player1 = if Enum.empty?(player2), do: player1 ++ war_cards, else: player1
        player2 = if Enum.empty?(player1), do: player2 ++ war_cards, else: player2
        {player1, player2}
      else

        {card1, card2, player1, player2} = deal_two_cards(player1, player2)
        war_cards = war_cards ++ [face_down1, face_down2, card1, card2]
        fight_war(player1, player2, war_cards)
      end
    end
  end

  defp remap_ace(card) do
    if card == 1, do: 14, else: card
  end

end
