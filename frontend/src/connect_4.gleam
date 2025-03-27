import lustre
import lustre/effect
import lustre/element

import messages as msg
import views/game as g
import views/main_menu as mm

pub fn main() {
  let app = lustre.application(new, update, view)
  let assert Ok(_) = lustre.start(app, "#app", msg.GotoMainMenu)

  Nil
}

// model

type Model {
  MainMenu
  Game(model: g.Model)
}

fn new(message: msg.Message) -> #(Model, effect.Effect(msg.Message)) {
  case message {
    msg.GotoMainMenu -> #(MainMenu, effect.none())
    msg.NewGame -> #(Game(g.new(g.Human, g.Human)), effect.none())
    msg.NewOnlineGame -> #(Game(g.new(g.Human, g.AI)), effect.none())
    _ -> panic as "should not happen"
  }
}

// message
fn update(
  model: Model,
  msg: msg.Message,
) -> #(Model, effect.Effect(msg.Message)) {
  case model, msg {
    // Nvaigation
    _, msg.GotoMainMenu -> #(MainMenu, effect.none())
    _, msg.NewGame -> new(msg)
    _, msg.NewOnlineGame -> new(msg)
    // Move updates
    Game(game_model), msg.Move(column) -> {
      let updated_game = g.update_game(game_model, column)
      case updated_game.state, updated_game.active.turn.player {
        // Trigger api call for ai move on AI turn
        g.Continue, g.AI -> #(Game(updated_game), g.get_move_api(updated_game))
        // Do not trigger api call in all other scenarios
        _, _ -> #(Game(updated_game), effect.none())
      }
    }
    Game(game_model), msg.ReceivedMove(result) -> {
      let assert Ok(column) = result
      let updated_game = g.update_game(game_model, column)
      #(Game(updated_game), effect.none())
    }
    // Column highlights
    Game(game_model), msg.HighlightColumn(column) -> {
      let updated_game = g.update_highlighted_column(game_model, column)
      #(Game(updated_game), effect.none())
    }
    Game(game_model), msg.UnhighlightColumn -> {
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

fn view(model: Model) -> element.Element(msg.Message) {
  case model {
    MainMenu -> mm.view()
    Game(game_model) -> g.view(game_model)
  }
}
