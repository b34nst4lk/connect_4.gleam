import gleam/dynamic/decode
import gleam/json

import lustre_http
import shared.{Red, Yellow}

import models.{type GameModel, APIReceivedMove}

pub fn get_move_api(model: GameModel, bot_name: String, game_id: Int) {
  let url = "http://localhost:8000/" <> bot_name
  // prepare json body
  let req_body =
    case model.game.active.turn {
      Red -> [
        #("red", json.int(model.game.active.board.val)),
        #("yellow", json.int(model.game.inactive.board.val)),
        #("play_for", json.string("red")),
        #("game_id", json.int(game_id)),
      ]
      Yellow -> [
        #("red", json.int(model.game.inactive.board.val)),
        #("yellow", json.int(model.game.active.board.val)),
        #("play_for", json.string("yellow")),
        #("game_id", json.int(game_id)),
      ]
    }
    |> json.object

  // prepare decoder
  let decoder = {
    use move <- decode.field("move", decode.int)
    use game_id <- decode.field("game_id", decode.int)
    decode.success(#(move, game_id))
  }
  lustre_http.post(
    url,
    req_body,
    lustre_http.expect_json(decoder, APIReceivedMove),
  )
}
