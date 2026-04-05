let spec = {|{
  "openapi": "3.1.0",
  "info": {
    "title": "sakura-mail",
    "description": "Local-first email API powered by mbsync and Maildir",
    "version": "0.1.0"
  },
  "servers": [
    { "url": "http://localhost:8080", "description": "Local development" }
  ],
  "components": {
    "securitySchemes": {
      "bearerAuth": {
        "type": "http",
        "scheme": "bearer",
        "description": "API key returned by POST /accounts (sk_...)"
      }
    },
    "schemas": {
      "ImapConfig": {
        "type": "object",
        "required": ["host", "port", "username", "password", "use_tls"],
        "properties": {
          "host": { "type": "string", "example": "imap.example.com" },
          "port": { "type": "integer", "example": 993 },
          "username": { "type": "string", "example": "user@example.com" },
          "password": { "type": "string", "example": "secret" },
          "use_tls": { "type": "boolean", "example": true }
        }
      },
      "Account": {
        "type": "object",
        "required": ["id", "host", "port", "username", "use_tls", "created_at"],
        "properties": {
          "id": { "type": "string", "format": "uuid" },
          "host": { "type": "string" },
          "port": { "type": "integer" },
          "username": { "type": "string" },
          "use_tls": { "type": "boolean" },
          "created_at": { "type": "string", "format": "date-time" }
        }
      },
      "MessageSummary": {
        "type": "object",
        "required": ["id", "from", "to", "subject", "mailbox", "flags", "is_new", "size", "has_attachments"],
        "properties": {
          "id": { "type": "string" },
          "from": { "type": "array", "items": { "$ref": "#/components/schemas/Address" } },
          "to": { "type": "array", "items": { "$ref": "#/components/schemas/Address" } },
          "subject": { "type": "string" },
          "date": { "type": "string", "format": "date-time", "nullable": true },
          "mailbox": { "type": "string" },
          "flags": { "type": "array", "items": { "type": "string", "enum": ["draft", "flagged", "replied", "seen", "trashed"] } },
          "is_new": { "type": "boolean" },
          "size": { "type": "integer" },
          "has_attachments": { "type": "boolean" }
        }
      },
      "MessageDetail": {
        "type": "object",
        "required": ["id", "from", "to", "cc", "bcc", "subject", "attachments", "flags", "mailbox", "size"],
        "properties": {
          "id": { "type": "string", "description": "Maildir message ID (used in URL paths)" },
          "message_id": { "type": "string", "nullable": true, "description": "RFC 822 Message-ID header" },
          "in_reply_to": { "type": "string", "nullable": true },
          "references": { "type": "array", "items": { "type": "string" } },
          "from": { "type": "array", "items": { "$ref": "#/components/schemas/Address" } },
          "to": { "type": "array", "items": { "$ref": "#/components/schemas/Address" } },
          "cc": { "type": "array", "items": { "$ref": "#/components/schemas/Address" } },
          "bcc": { "type": "array", "items": { "$ref": "#/components/schemas/Address" } },
          "subject": { "type": "string" },
          "date": { "type": "string", "format": "date-time", "nullable": true },
          "body_text": { "type": "string", "nullable": true },
          "body_html": { "type": "string", "nullable": true },
          "attachments": { "type": "array", "items": { "$ref": "#/components/schemas/AttachmentInfo" } },
          "flags": { "type": "array", "items": { "type": "string", "enum": ["draft", "flagged", "replied", "seen", "trashed"] } },
          "mailbox": { "type": "string" },
          "size": { "type": "integer" }
        }
      },
      "Address": {
        "type": "object",
        "required": ["email"],
        "properties": {
          "name": { "type": "string", "nullable": true },
          "email": { "type": "string" }
        }
      },
      "AttachmentInfo": {
        "type": "object",
        "required": ["index", "content_type", "size"],
        "properties": {
          "index": { "type": "integer" },
          "filename": { "type": "string", "nullable": true },
          "content_type": { "type": "string" },
          "size": { "type": "integer" }
        }
      },
      "Draft": {
        "type": "object",
        "required": ["to", "subject", "body"],
        "properties": {
          "to": { "type": "array", "items": { "type": "string" } },
          "cc": { "type": "array", "items": { "type": "string" }, "default": [] },
          "bcc": { "type": "array", "items": { "type": "string" }, "default": [] },
          "subject": { "type": "string" },
          "body": { "type": "string" },
          "in_reply_to": { "type": "string", "nullable": true }
        }
      },
      "Settings": {
        "type": "object",
        "properties": {
          "bulk_max": { "type": "integer", "description": "Maximum messages per bulk operation" },
          "sync_interval_sec": { "type": "number", "description": "Auto-sync interval in seconds" },
          "default_page_limit": { "type": "integer", "description": "Default page size for message listings" }
        }
      },
      "SyncLogEntry": {
        "type": "object",
        "required": ["timestamp", "status", "exit_code", "output"],
        "properties": {
          "timestamp": { "type": "string", "format": "date-time" },
          "status": { "type": "string", "enum": ["success", "failed"] },
          "exit_code": { "type": "integer" },
          "output": { "type": "string" }
        }
      },
      "Error": {
        "type": "object",
        "required": ["error"],
        "properties": {
          "error": { "type": "string" }
        }
      }
    }
  },
  "security": [{ "bearerAuth": [] }],
  "paths": {
    "/health": {
      "get": {
        "operationId": "healthCheck",
        "summary": "Health check",
        "description": "No authentication required",
        "tags": ["System"],
        "security": [],
        "responses": {
          "200": {
            "description": "Server is running",
            "content": { "application/json": { "schema": { "type": "object", "properties": { "status": { "type": "string", "example": "ok" } } } } }
          }
        }
      }
    },
    "/stats": {
      "get": {
        "operationId": "getStats",
        "summary": "Server statistics",
        "tags": ["System"],
        "responses": {
          "200": {
            "description": "Server stats",
            "content": { "application/json": { "schema": { "type": "object", "properties": {
              "status": { "type": "string" },
              "accounts": { "type": "integer" },
              "uptime_seconds": { "type": "integer" },
              "last_sync": { "type": "object", "nullable": true, "properties": { "account_id": { "type": "string" }, "at": { "type": "string", "format": "date-time" } } },
              "maildir": { "type": "object", "properties": { "total_messages": { "type": "integer" }, "total_size_bytes": { "type": "integer" } } },
              "settings": { "$ref": "#/components/schemas/Settings" }
            } } } }
          },
          "401": { "description": "Unauthorized", "content": { "application/json": { "schema": { "$ref": "#/components/schemas/Error" } } } }
        }
      }
    },
    "/openapi.json": {
      "get": {
        "operationId": "getOpenApiSpec",
        "summary": "OpenAPI specification",
        "tags": ["System"],
        "security": [],
        "responses": {
          "200": { "description": "This document", "content": { "application/json": {} } }
        }
      }
    },
    "/settings": {
      "get": {
        "operationId": "getSettings",
        "summary": "Get runtime settings",
        "tags": ["Settings"],
        "responses": {
          "200": { "description": "Current settings", "content": { "application/json": { "schema": { "$ref": "#/components/schemas/Settings" } } } },
          "401": { "description": "Unauthorized" }
        }
      },
      "patch": {
        "operationId": "updateSettings",
        "summary": "Update runtime settings",
        "description": "Partial updates supported. Invalid values (negative, zero) are silently ignored. Changes do not persist across restarts.",
        "tags": ["Settings"],
        "requestBody": { "content": { "application/json": { "schema": { "$ref": "#/components/schemas/Settings" } } } },
        "responses": {
          "200": { "description": "Updated settings", "content": { "application/json": { "schema": { "$ref": "#/components/schemas/Settings" } } } },
          "401": { "description": "Unauthorized" }
        }
      }
    },
    "/verify": {
      "post": {
        "operationId": "verifyImapConnection",
        "summary": "Verify IMAP connection",
        "description": "Tests IMAP credentials without creating an account. Returns available mailboxes on success. No authentication required.",
        "tags": ["Accounts"],
        "security": [],
        "requestBody": { "required": true, "content": { "application/json": { "schema": { "$ref": "#/components/schemas/ImapConfig" } } } },
        "responses": {
          "200": { "description": "Connection successful", "content": { "application/json": { "schema": { "type": "object", "properties": { "ok": { "type": "boolean" }, "mailboxes": { "type": "array", "items": { "type": "string" } } } } } } },
          "400": { "description": "Connection failed or invalid body", "content": { "application/json": { "schema": { "$ref": "#/components/schemas/Error" } } } }
        }
      }
    },
    "/accounts": {
      "get": {
        "operationId": "listAccounts",
        "summary": "List accounts",
        "description": "Returns accounts accessible by the authenticated API key (currently only the owning account).",
        "tags": ["Accounts"],
        "responses": {
          "200": { "description": "Account list", "content": { "application/json": { "schema": { "type": "array", "items": { "$ref": "#/components/schemas/Account" } } } } },
          "401": { "description": "Unauthorized" }
        }
      },
      "post": {
        "operationId": "registerAccount",
        "summary": "Register account",
        "description": "Creates a new account with IMAP credentials. Returns the account ID and API key. No authentication required.",
        "tags": ["Accounts"],
        "security": [],
        "requestBody": { "required": true, "content": { "application/json": { "schema": { "$ref": "#/components/schemas/ImapConfig" } } } },
        "responses": {
          "201": { "description": "Account created", "content": { "application/json": { "schema": { "type": "object", "properties": { "id": { "type": "string" }, "api_key": { "type": "string" } } } } } },
          "400": { "description": "Invalid body", "content": { "application/json": { "schema": { "$ref": "#/components/schemas/Error" } } } }
        }
      },
      "delete": {
        "operationId": "deleteAccount",
        "summary": "Delete account",
        "description": "Deletes the authenticated account and all its data. The account is identified by the bearer token, not a path parameter.",
        "tags": ["Accounts"],
        "responses": {
          "200": { "description": "Account deleted", "content": { "application/json": { "schema": { "type": "object", "properties": { "ok": { "type": "boolean" } } } } } },
          "401": { "description": "Unauthorized" }
        }
      }
    },
    "/accounts/{id}": {
      "get": {
        "operationId": "getAccount",
        "summary": "Get account",
        "tags": ["Accounts"],
        "parameters": [{ "name": "id", "in": "path", "required": true, "schema": { "type": "string" } }],
        "responses": {
          "200": { "description": "Account details", "content": { "application/json": { "schema": { "$ref": "#/components/schemas/Account" } } } },
          "404": { "description": "Account not found" },
          "401": { "description": "Unauthorized" }
        }
      },
      "put": {
        "operationId": "updateAccount",
        "summary": "Update account IMAP config",
        "tags": ["Accounts"],
        "parameters": [
          { "name": "id", "in": "path", "required": true, "schema": { "type": "string" } },
          { "name": "sync", "in": "query", "schema": { "type": "boolean" }, "description": "Trigger sync after update" }
        ],
        "requestBody": { "required": true, "content": { "application/json": { "schema": { "$ref": "#/components/schemas/ImapConfig" } } } },
        "responses": {
          "200": { "description": "Updated account", "content": { "application/json": { "schema": { "$ref": "#/components/schemas/Account" } } } },
          "400": { "description": "Invalid body" },
          "404": { "description": "Account not found" },
          "401": { "description": "Unauthorized" }
        }
      }
    },
    "/accounts/reroll-key": {
      "post": {
        "operationId": "rerollApiKey",
        "summary": "Reroll API key",
        "description": "Generates a new API key. The old key is immediately invalidated.",
        "tags": ["Accounts"],
        "responses": {
          "200": { "description": "New API key", "content": { "application/json": { "schema": { "type": "object", "properties": { "api_key": { "type": "string" } } } } } },
          "401": { "description": "Unauthorized" }
        }
      }
    },
    "/accounts/{id}/logs": {
      "get": {
        "operationId": "getSyncLogs",
        "summary": "Get sync logs",
        "description": "Returns sync log entries for the account, newest first.",
        "tags": ["Sync"],
        "parameters": [
          { "name": "id", "in": "path", "required": true, "schema": { "type": "string" } },
          { "name": "limit", "in": "query", "schema": { "type": "integer", "default": 50 }, "description": "Maximum entries to return" }
        ],
        "responses": {
          "200": { "description": "Sync log entries", "content": { "application/json": { "schema": { "type": "array", "items": { "$ref": "#/components/schemas/SyncLogEntry" } } } } },
          "404": { "description": "Account not found" },
          "401": { "description": "Unauthorized" }
        }
      }
    },
    "/sync": {
      "post": {
        "operationId": "triggerSync",
        "summary": "Trigger sync",
        "description": "Runs mbsync for the authenticated account.",
        "tags": ["Sync"],
        "responses": {
          "200": { "description": "Sync completed", "content": { "application/json": { "schema": { "type": "object", "properties": { "ok": { "type": "boolean" }, "message": { "type": "string" } } } } } },
          "409": { "description": "Sync already in progress" },
          "500": { "description": "Sync failed" },
          "401": { "description": "Unauthorized" }
        }
      }
    },
    "/mailboxes": {
      "get": {
        "operationId": "listMailboxes",
        "summary": "List mailboxes",
        "tags": ["Mailboxes"],
        "responses": {
          "200": { "description": "Mailbox names", "content": { "application/json": { "schema": { "type": "array", "items": { "type": "string" } } } } },
          "401": { "description": "Unauthorized" }
        }
      },
      "post": {
        "operationId": "createMailbox",
        "summary": "Create mailbox",
        "tags": ["Mailboxes"],
        "requestBody": { "required": true, "content": { "application/json": { "schema": { "type": "object", "required": ["name"], "properties": { "name": { "type": "string" } } } } } },
        "responses": {
          "201": { "description": "Mailbox created", "content": { "application/json": { "schema": { "type": "object", "properties": { "name": { "type": "string" } } } } } },
          "400": { "description": "Missing name" },
          "409": { "description": "Mailbox already exists" },
          "401": { "description": "Unauthorized" }
        }
      }
    },
    "/mailboxes/{name}": {
      "delete": {
        "operationId": "deleteMailbox",
        "summary": "Delete mailbox",
        "description": "Mailbox must be empty.",
        "tags": ["Mailboxes"],
        "parameters": [{ "name": "name", "in": "path", "required": true, "schema": { "type": "string" } }],
        "responses": {
          "200": { "description": "Mailbox deleted", "content": { "application/json": { "schema": { "type": "object", "properties": { "ok": { "type": "boolean" } } } } } },
          "404": { "description": "Mailbox not found" },
          "409": { "description": "Mailbox not empty" },
          "401": { "description": "Unauthorized" }
        }
      },
      "put": {
        "operationId": "renameMailbox",
        "summary": "Rename mailbox",
        "description": "Renames a local mailbox. Does not affect the IMAP server.",
        "tags": ["Mailboxes"],
        "parameters": [{ "name": "name", "in": "path", "required": true, "schema": { "type": "string" } }],
        "requestBody": { "required": true, "content": { "application/json": { "schema": { "type": "object", "required": ["name"], "properties": { "name": { "type": "string", "description": "New mailbox name" } } } } } },
        "responses": {
          "200": { "description": "Mailbox renamed", "content": { "application/json": { "schema": { "type": "object", "properties": { "name": { "type": "string" } } } } } },
          "404": { "description": "Source mailbox not found" },
          "409": { "description": "Destination mailbox already exists" },
          "400": { "description": "Missing name" },
          "401": { "description": "Unauthorized" }
        }
      }
    },
    "/mailboxes/{mailbox}/messages": {
      "get": {
        "operationId": "listMessages",
        "summary": "List messages in mailbox",
        "tags": ["Messages"],
        "parameters": [
          { "name": "mailbox", "in": "path", "required": true, "schema": { "type": "string" } },
          { "name": "limit", "in": "query", "schema": { "type": "integer", "default": 50 }, "description": "Page size (<=0 falls back to default)" },
          { "name": "cursor", "in": "query", "schema": { "type": "string" }, "description": "Pagination cursor from previous response" },
          { "name": "sort", "in": "query", "schema": { "type": "string", "example": "date:desc,from:asc" }, "description": "Comma-separated sort fields (date, from, subject, size) with :asc or :desc" },
          { "name": "from", "in": "query", "schema": { "type": "string" }, "description": "Filter by sender (substring, case-insensitive)" },
          { "name": "to", "in": "query", "schema": { "type": "string" }, "description": "Filter by recipient" },
          { "name": "subject", "in": "query", "schema": { "type": "string" }, "description": "Filter by subject" },
          { "name": "body", "in": "query", "schema": { "type": "string" }, "description": "Filter by body content" },
          { "name": "flags", "in": "query", "schema": { "type": "string" }, "description": "Filter by flag name" },
          { "name": "unread", "in": "query", "schema": { "type": "boolean" }, "description": "Filter by read/unread status" },
          { "name": "has_attachment", "in": "query", "schema": { "type": "boolean" }, "description": "Filter messages with attachments" },
          { "name": "after", "in": "query", "schema": { "type": "string", "format": "date-time" }, "description": "Messages after this date" },
          { "name": "before", "in": "query", "schema": { "type": "string", "format": "date-time" }, "description": "Messages before this date" }
        ],
        "responses": {
          "200": { "description": "Paginated message list", "content": { "application/json": { "schema": { "type": "object", "properties": {
            "messages": { "type": "array", "items": { "$ref": "#/components/schemas/MessageSummary" } },
            "total": { "type": "integer" },
            "limit": { "type": "integer" },
            "next_cursor": { "type": "string", "nullable": true },
            "prev_cursor": { "type": "string", "nullable": true }
          } } } } },
          "404": { "description": "Mailbox not found" },
          "401": { "description": "Unauthorized" }
        }
      }
    },
    "/messages/{id}": {
      "get": {
        "operationId": "getMessage",
        "summary": "Get message detail",
        "tags": ["Messages"],
        "parameters": [{ "name": "id", "in": "path", "required": true, "schema": { "type": "string" } }],
        "responses": {
          "200": { "description": "Message detail", "content": { "application/json": { "schema": { "$ref": "#/components/schemas/MessageDetail" } } } },
          "404": { "description": "Message not found" },
          "401": { "description": "Unauthorized" }
        }
      },
      "delete": {
        "operationId": "deleteMessage",
        "summary": "Delete message",
        "description": "Moves message to Trash (soft delete).",
        "tags": ["Messages"],
        "parameters": [{ "name": "id", "in": "path", "required": true, "schema": { "type": "string" } }],
        "responses": {
          "200": { "description": "Message trashed", "content": { "application/json": { "schema": { "type": "object", "properties": { "ok": { "type": "boolean" } } } } } },
          "404": { "description": "Message not found" },
          "401": { "description": "Unauthorized" }
        }
      }
    },
    "/messages/{id}/raw": {
      "get": {
        "operationId": "getRawMessage",
        "summary": "Get raw RFC 5322 message",
        "tags": ["Messages"],
        "parameters": [{ "name": "id", "in": "path", "required": true, "schema": { "type": "string" } }],
        "responses": {
          "200": { "description": "Raw email content", "content": { "message/rfc822": { "schema": { "type": "string", "format": "binary" } } } },
          "404": { "description": "Message not found" },
          "401": { "description": "Unauthorized" }
        }
      }
    },
    "/messages/{id}/attachments/{index}": {
      "get": {
        "operationId": "downloadAttachment",
        "summary": "Download attachment",
        "tags": ["Messages"],
        "parameters": [
          { "name": "id", "in": "path", "required": true, "schema": { "type": "string" } },
          { "name": "index", "in": "path", "required": true, "schema": { "type": "integer" } }
        ],
        "responses": {
          "200": { "description": "Attachment content with appropriate Content-Type and Content-Disposition headers" },
          "400": { "description": "Invalid index" },
          "404": { "description": "Message or attachment not found" },
          "401": { "description": "Unauthorized" }
        }
      }
    },
    "/messages/{id}/flags": {
      "put": {
        "operationId": "setMessageFlags",
        "summary": "Set message flags",
        "description": "Replaces all flags on the message. Valid flags: draft, flagged, replied, seen, trashed.",
        "tags": ["Messages"],
        "parameters": [{ "name": "id", "in": "path", "required": true, "schema": { "type": "string" } }],
        "requestBody": { "required": true, "content": { "application/json": { "schema": { "type": "object", "required": ["flags"], "properties": { "flags": { "type": "array", "items": { "type": "string", "enum": ["draft", "flagged", "replied", "seen", "trashed"] } } } } } } },
        "responses": {
          "200": { "description": "Flags updated", "content": { "application/json": { "schema": { "type": "object", "properties": { "ok": { "type": "boolean" } } } } } },
          "400": { "description": "Invalid body or unknown flags" },
          "404": { "description": "Message not found" },
          "401": { "description": "Unauthorized" }
        }
      }
    },
    "/messages/{id}/move": {
      "post": {
        "operationId": "moveMessage",
        "summary": "Move message to another mailbox",
        "description": "Moves a message to the target mailbox. Note: the message ID may change after a move since Maildir encodes location in the filename.",
        "tags": ["Messages"],
        "parameters": [{ "name": "id", "in": "path", "required": true, "schema": { "type": "string" } }],
        "requestBody": { "required": true, "content": { "application/json": { "schema": { "type": "object", "required": ["destination"], "properties": { "destination": { "type": "string", "description": "Target mailbox name" } } } } } },
        "responses": {
          "200": { "description": "Message moved", "content": { "application/json": { "schema": { "type": "object", "properties": { "ok": { "type": "boolean" } } } } } },
          "400": { "description": "Invalid body" },
          "404": { "description": "Message not found" },
          "401": { "description": "Unauthorized" }
        }
      }
    },
    "/messages/bulk": {
      "post": {
        "operationId": "bulkMessageOperation",
        "summary": "Bulk message operations",
        "description": "Apply an action to multiple messages. Provide either 'ids' (explicit message IDs) or 'filter' + 'mailbox' (query-based selection), but not both.",
        "tags": ["Messages"],
        "requestBody": { "required": true, "content": { "application/json": { "schema": { "type": "object", "required": ["action"], "properties": {
          "ids": { "type": "array", "items": { "type": "string" }, "description": "Explicit message IDs (mutually exclusive with filter+mailbox)" },
          "action": { "type": "string", "enum": ["set_flags", "add_flags", "remove_flags", "move", "delete"] },
          "params": { "type": "object", "description": "Action parameters (flags list or destination)", "properties": {
            "flags": { "type": "array", "items": { "type": "string", "enum": ["draft", "flagged", "replied", "seen", "trashed"] } },
            "destination": { "type": "string" }
          } },
          "mailbox": { "type": "string", "description": "Mailbox to filter (required with filter, mutually exclusive with ids)" },
          "filter": { "type": "object", "description": "Filter criteria (same as message listing query params, mutually exclusive with ids)", "properties": {
            "from": { "type": "string" },
            "to": { "type": "string" },
            "subject": { "type": "string" },
            "body": { "type": "string" },
            "flags": { "type": "string" },
            "unread": { "type": "boolean" },
            "has_attachment": { "type": "boolean" },
            "after": { "type": "string", "format": "date-time" },
            "before": { "type": "string", "format": "date-time" }
          } }
        } } } } },
        "responses": {
          "200": { "description": "Bulk operation results", "content": { "application/json": { "schema": { "type": "object", "properties": {
            "succeeded": { "type": "array", "items": { "type": "string" } },
            "failed": { "type": "array", "items": { "type": "object", "properties": { "id": { "type": "string" }, "error": { "type": "string" } } } }
          } } } } },
          "400": { "description": "Invalid action, missing IDs/filter, or over bulk_max limit" },
          "401": { "description": "Unauthorized" }
        }
      }
    },
    "/drafts": {
      "post": {
        "operationId": "createDraft",
        "summary": "Create draft",
        "tags": ["Drafts"],
        "requestBody": { "required": true, "content": { "application/json": { "schema": { "$ref": "#/components/schemas/Draft" } } } },
        "responses": {
          "201": { "description": "Draft created", "content": { "application/json": { "schema": { "type": "object", "properties": { "id": { "type": "string" }, "mailbox": { "type": "string" } } } } } },
          "400": { "description": "Invalid body" },
          "401": { "description": "Unauthorized" }
        }
      }
    },
    "/drafts/{id}": {
      "put": {
        "operationId": "updateDraft",
        "summary": "Update draft",
        "description": "Replaces the draft content. Returns a new message ID — the old ID becomes invalid after this call.",
        "tags": ["Drafts"],
        "parameters": [{ "name": "id", "in": "path", "required": true, "schema": { "type": "string" } }],
        "requestBody": { "required": true, "content": { "application/json": { "schema": { "$ref": "#/components/schemas/Draft" } } } },
        "responses": {
          "200": { "description": "Draft updated (note: ID has changed)", "content": { "application/json": { "schema": { "type": "object", "properties": { "id": { "type": "string", "description": "New message ID (old ID is now invalid)" }, "mailbox": { "type": "string" } } } } } },
          "400": { "description": "Invalid body" },
          "404": { "description": "Draft not found" },
          "401": { "description": "Unauthorized" }
        }
      }
    }
  },
  "tags": [
    { "name": "System", "description": "Health, stats, and OpenAPI spec" },
    { "name": "Settings", "description": "Runtime configuration" },
    { "name": "Accounts", "description": "Account registration and management" },
    { "name": "Sync", "description": "IMAP synchronization" },
    { "name": "Mailboxes", "description": "Mailbox listing and management" },
    { "name": "Messages", "description": "Message reading, search, flags, move, delete, and bulk operations" },
    { "name": "Drafts", "description": "Draft composition" }
  ]
}|}

let handle_openapi _request =
  Dream.respond
    ~headers:[("Content-Type", "application/json")]
    spec
