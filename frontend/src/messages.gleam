import lustre_http

pub type Message {
  // Main menu
  GotoMainMenu

  // New local play
  NewGame

  // Vs AI
  NewOnlineGame

  // Shared messages
  Move(column: Int)
  ReceivedMove(Result(Int, lustre_http.HttpError))
  HighlightColumn(column: Int)
  UnhighlightColumn
}
