import gleam/dict

import lustre
import lustre/effect
import lustre/element

import shared.{Continue}

import backend_api as api
import models.{
  type GameModel, type Message, AI, APIReceivedMove, HighlightColumn, Human,
  Move, NewGame, UnhighlightColumn, UserClickedMainMenu,
  UserEnteredNumberOfGames, UserSelectedBot, UserStartedAIBattle,
  get_active_player_type, new_game, update_model,
}
import views/ai_battle as ai
import views/game as g
import views/main_menu as mm

pub fn main() {
  let app = lustre.application(new, update, view)
  let assert Ok(_) = lustre.start(app, "#app", UserClickedMainMenu)

  Nil
}

// model

type Model {
  MainMenu(model: mm.Model)
  Game(model: GameModel)
  AIBattle(model: ai.Model)
}

fn new(message: Message) -> #(Model, effect.Effect(Message)) {
  case message {
    UserClickedMainMenu -> #(MainMenu(mm.new()), effect.none())
    NewGame(player_types) -> {
      let new_game = new_game(player_types.red, player_types.yellow)
      #(Game(new_game), case player_types.red {
        Human -> effect.none()
        AI(bot_name) -> api.get_move_api(new_game, bot_name, -1)
      })
    }

    _ -> panic as "should not happen"
  }
}

// message
fn update(model: Model, msg: Message) -> #(Model, effect.Effect(Message)) {
  case model, msg {
    // Navigation
    _, UserClickedMainMenu -> #(MainMenu(mm.new()), effect.none())
    _, NewGame(_) -> new(msg)
    MainMenu(model), UserSelectedBot(turn, bot_name) -> #(
      MainMenu(mm.update_selected_bot(model, turn, bot_name)),
      effect.none(),
    )
    MainMenu(model), UserEnteredNumberOfGames(number_of_games) -> #(
      MainMenu(mm.update_number_of_games(model, number_of_games)),
      effect.none(),
    )

    // Game move updates
    Game(game_model), Move(column) -> {
      let updated_game = update_model(game_model, column)
      case updated_game.game.state, get_active_player_type(updated_game) {
        // Trigger api call for ai move on AI turn
        Continue, AI(bot_name) -> #(
          Game(updated_game),
          api.get_move_api(updated_game, bot_name, -1),
        )
        // Do not trigger api call in all other scenarios
        _, _ -> #(Game(updated_game), effect.none())
      }
    }
    Game(game_model), APIReceivedMove(result) -> {
      let assert Ok(#(column, _)) = result
      let updated_game = update_model(game_model, column)
      case updated_game.game.state, get_active_player_type(updated_game) {
        // Trigger api call for ai move on AI turn
        Continue, AI(bot_name) -> #(
          Game(updated_game),
          api.get_move_api(updated_game, bot_name, -1),
        )
        // Do not trigger api call in all other scenarios
        _, _ -> #(Game(updated_game), effect.none())
      }
    }

    // Column highlights
    Game(game_model), HighlightColumn(column) -> {
      let updated_game = g.update_highlighted_column(game_model, column)
      #(Game(updated_game), effect.none())
    }
    Game(game_model), UnhighlightColumn -> {
      let updated_game = g.update_clear_highlighted_column(game_model)
      #(Game(updated_game), effect.none())
    }

    // AI Battle
    _, UserStartedAIBattle(red, yellow, number_of_games) -> {
      let new_battle = ai.new(red, yellow, number_of_games)
      #(AIBattle(new_battle), ai.init_get_first_moves(red, new_battle))
    }
    AIBattle(model), APIReceivedMove(result) -> {
      let assert Ok(#(column, game_id)) = result
      let assert Ok(game) = dict.get(model.games, game_id)
      let updated_game = update_model(game, column)
      let updated_games =
        AIBattle(
          ai.Model(
            ..model,
            games: dict.insert(model.games, game_id, updated_game),
          ),
        )
      case updated_game.game.state, get_active_player_type(updated_game) {
        Continue, AI(bot_name) -> #(
          updated_games,
          api.get_move_api(updated_game, bot_name, game_id),
        )
        _, _ -> #(updated_games, effect.none())
      }
    }

    // Unhandled/impossible states
    _, _ -> {
      echo model
      echo msg
      panic as "impossible state"
    }
  }
}

// view

fn view(model: Model) -> element.Element(_) {
  case model {
    MainMenu(model) -> mm.view(model)
    Game(game_model) -> g.view(game_model)
    AIBattle(model) -> ai.view(model)
  }
}
