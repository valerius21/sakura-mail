let routes ~registry ~sync_scheduler ~base_dir ~settings =
  [
    Dream.get "/openapi.json" (fun request -> Openapi.handle_openapi request);
    Dream.get "/health" (fun request -> Health_handler.handle_health request);
    Dream.get "/stats"
      (fun request ->
        Health_handler.handle_stats registry sync_scheduler base_dir settings request);

    Dream.get "/settings" (fun request ->
      Settings_handler.handle_get registry settings request);
    Dream.patch "/settings" (fun request ->
      Settings_handler.handle_update registry settings request);

    Dream.post "/verify" (fun request -> Verify_handler.handle_verify request);

    Dream.get "/accounts" (fun request ->
      Account_handler.handle_list registry request);
    Dream.post "/accounts" (Account_handler.handle_register registry);
    Dream.get "/accounts/:id" (fun request ->
      let id = Dream.param request "id" in
      Account_handler.handle_get registry request id);
    Dream.put "/accounts/:id" (fun request ->
      let id = Dream.param request "id" in
      Account_handler.handle_update registry sync_scheduler base_dir request id);
    Dream.delete "/accounts" (Account_handler.handle_delete registry);
    Dream.post "/accounts/reroll-key" (Account_handler.handle_reroll_key registry);

    Dream.get "/accounts/:id/logs" (fun request ->
      let id = Dream.param request "id" in
      Log_handler.handle_logs registry base_dir request id);

    Dream.post "/sync" (Sync_handler.handle_sync sync_scheduler registry base_dir);

    Dream.get "/mailboxes" (Mailbox_handler.handle_list registry base_dir);
    Dream.post "/mailboxes"
      (fun request ->
        Mailbox_handler.handle_create registry sync_scheduler base_dir request);
    Dream.delete "/mailboxes/:name"
      (fun request ->
        let name = Dream.param request "name" in
        Mailbox_handler.handle_delete_mailbox registry sync_scheduler base_dir request name);
    Dream.put "/mailboxes/:name"
      (fun request ->
        let name = Dream.param request "name" in
        Mailbox_handler.handle_rename registry sync_scheduler base_dir request name);
    Dream.get "/mailboxes/:mailbox/messages"
      (fun request ->
        let mailbox = Dream.param request "mailbox" in
        Mailbox_handler.handle_messages settings registry base_dir request mailbox);

    Dream.get "/messages/:id"
      (fun request ->
        let id = Dream.param request "id" in
        Message_handler.handle_read registry base_dir request id);
    Dream.get "/messages/:id/raw"
      (fun request ->
        let id = Dream.param request "id" in
        Message_handler.handle_raw registry base_dir request id);
    Dream.get "/messages/:id/attachments/:index"
      (fun request ->
        let id = Dream.param request "id" in
        let index = Dream.param request "index" in
        Message_handler.handle_attachment registry base_dir request id index);
    Dream.put "/messages/:id/flags"
      (fun request ->
        let id = Dream.param request "id" in
        Message_handler.handle_set_flags registry base_dir request id);
    Dream.post "/messages/:id/move"
      (fun request ->
        let id = Dream.param request "id" in
        Message_handler.handle_move registry base_dir request id);
    Dream.delete "/messages/:id"
      (fun request ->
        let id = Dream.param request "id" in
        Message_handler.handle_delete registry base_dir request id);

    Dream.post "/messages/bulk"
      (fun request -> Bulk_handler.handle_bulk settings registry base_dir request);

    Dream.post "/drafts" (Draft_handler.handle_create registry base_dir);
    Dream.put "/drafts/:id"
      (fun request ->
        let id = Dream.param request "id" in
        Draft_handler.handle_update registry base_dir request id);
  ]
