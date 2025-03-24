import lustre
import lustre/effect
import lustre/element

import messages as msg
import views/game_board as gb
import views/main_menu as mm
import views/vs_ai_board as og

pub fn main() {
  let app = lustre.application(new, update, view)
  let assert Ok(_) = lustre.start(app, "#app", msg.GotoMainMenu)

  Nil
}

// model

type Model {
  MainMenu
  LocalGame(model: gb.Model)
  OnlineGame(model: og.Model)
}

fn new(message: msg.Message) -> #(Model, effect.Effect(msg.Message)) {
  case message {
    msg.GotoMainMenu -> #(MainMenu, effect.none())
    msg.NewGame -> #(LocalGame(gb.new()), effect.none())
    msg.NewOnlineGame -> #(OnlineGame(og.new()), effect.none())
    _ -> panic as "should not happen"
  }
}

// message
fn update(
  model: Model,
  msg: msg.Message,
) -> #(Model, effect.Effect(msg.Message)) {
  case model, msg {
    _, msg.GotoMainMenu -> #(MainMenu, effect.none())
    _, msg.NewGame -> new(msg)
    _, msg.NewOnlineGame -> new(msg)
    LocalGame(game_model), msg.Move(column) -> #(
      LocalGame(gb.update_game(game_model, column)),
      effect.none(),
    )
    OnlineGame(game_model), msg.Move(column) -> {
      let updated_game = og.update_game(game_model, column)
      case updated_game.state, updated_game.active.turn.player {
        // Trigger api call for ai move on AI turn
        og.Continue, og.AI -> #(
          OnlineGame(updated_game),
          og.get_move_api(updated_game),
        )
        // Do not trigger api call in all other scenarios
        _, _ -> #(OnlineGame(updated_game), effect.none())
      }
    }
    OnlineGame(game_model), msg.ReceivedMove(result) -> {
      let assert Ok(column) = result
      let updated_game = og.update_game(game_model, column)
      #(OnlineGame(updated_game), effect.none())
    }
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
    LocalGame(game_model) -> gb.view(game_model)
    OnlineGame(game_model) -> og.view(game_model)
  }
}
