import lustre
import lustre/element

import messages as msg
import views/game_board as gb
import views/main_menu as mm

pub fn main() {
  let app = lustre.simple(new, update, view)
  let assert Ok(_) = lustre.start(app, "#app", msg.GotoMainMenu)

  Nil
}

// model

type Model {
  MainMenu
  Game(model: gb.Model)
}

fn new(message: msg.Message) -> Model {
  case message {
    msg.GotoMainMenu -> MainMenu
    msg.NewGame -> Game(gb.new())
    _ -> panic as "should not happen"
  }
}

// message
fn update(model: Model, msg: msg.Message) -> Model {
  case model, msg {
    _, msg.GotoMainMenu -> MainMenu
    _, msg.NewGame -> new(msg)
    Game(game_model), msg.Move(column) ->
      Game(gb.update_game(game_model, column))
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
    Game(game_model) -> gb.view(game_model)
  }
}
