import gleam/dict.{type Dict}

import lustre_http

import shared.{
  type Game, type GameState, type Turn, Continue, Game, Player, Red, Yellow,
  connect_4_height, connect_4_width,
}

import bibi/bitboard as b

pub type DebugLog {
  DebugLog(move_count: Int, turn: Turn, state: GameState)
}

pub type PlayerType {
  Human
  AI
}

pub type PlayerTypes {
  PlayerTypes(red: PlayerType, yellow: PlayerType)
}

pub type GameModel {
  GameModel(
    game: Game,
    player_types: PlayerTypes,
    move_counter: Int,
    move_history: Dict(Int, DebugLog),
    highlight_column: Int,
  )
}

pub fn get_active_player_type(model: GameModel) -> PlayerType {
  case model.game.active.turn {
    Red -> model.player_types.red
    Yellow -> model.player_types.yellow
  }
}

pub fn new_game(red: PlayerType, yellow: PlayerType) -> GameModel {
  let assert Ok(bitboard) = b.new(connect_4_width, connect_4_height)
  GameModel(
    Game(Player(Red, bitboard), Player(Yellow, bitboard), Continue),
    PlayerTypes(red, yellow),
    0,
    dict.new(),
    -1,
  )
}

pub type Message {
  // Main menu
  GotoMainMenu
  SetupAIvsAI

  // New Game
  NewGame(player_types: PlayerTypes)

  // Shared messages
  Move(column: Int)
  ReceivedMove(Result(Int, lustre_http.HttpError))
  HighlightColumn(column: Int)
  UnhighlightColumn
}
