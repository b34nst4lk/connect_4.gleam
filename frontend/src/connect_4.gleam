import lustre
import lustre/effect
import lustre/element

import shared.{Continue}

import models.{
  type GameModel, type Message, AI, GotoMainMenu, HighlightColumn, Human, Move,
  NewGame, ReceivedMove, UnhighlightColumn, get_active_player_type, new_game,
}
import views/game as g
import views/main_menu as mm

pub fn main() {
  let app = lustre.application(new, update, view)
  let assert Ok(_) = lustre.start(app, "#app", GotoMainMenu)

  Nil
}

// model

type Model {
  MainMenu
  Game(model: GameModel)
}

fn new(message: Message) -> #(Model, effect.Effect(Message)) {
  case message {
    GotoMainMenu -> #(MainMenu, effect.none())
    NewGame(player_types) -> {
      let new_game = new_game(player_types.red, player_types.yellow)
      #(Game(new_game), case player_types.red {
        Human -> effect.none()
        AI -> g.get_move_api(new_game)
      })
    }

    _ -> panic as "should not happen"
  }
}

// message
fn update(model: Model, msg: Message) -> #(Model, effect.Effect(Message)) {
  case model, msg {
    // Nvaigation
    _, GotoMainMenu -> #(MainMenu, effect.none())
    _, NewGame(_) -> new(msg)
    // Move updates
    Game(game_model), Move(column) -> {
      let updated_game = g.update_model(game_model, column)
      case updated_game.game.state, get_active_player_type(updated_game) {
        // Trigger api call for ai move on AI turn
        Continue, AI -> #(Game(updated_game), g.get_move_api(updated_game))
        // Do not trigger api call in all other scenarios
        _, _ -> #(Game(updated_game), effect.none())
      }
    }
    Game(game_model), ReceivedMove(result) -> {
      let assert Ok(column) = result
      let updated_game = g.update_model(game_model, column)
      case updated_game.game.state, get_active_player_type(updated_game) {
        // Trigger api call for ai move on AI turn
        Continue, AI -> #(Game(updated_game), g.get_move_api(updated_game))
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
    MainMenu -> mm.view()
    Game(game_model) -> g.view(game_model)
  }
}
