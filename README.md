# sakura-mail

HTTP API for email, built on OCaml. Wraps [mbsync](https://isync.sourceforge.io/mbsync.html) for IMAP synchronization and stores mail locally in Maildir format.

Tests and docs/openapi.json are AI generated.

## Prerequisites

- OCaml 5.x with opam
- [mbsync](https://isync.sourceforge.io/mbsync.html) (isync)
- Docker (for test IMAP server)
- Node.js (for test seed script)

## Install dependencies

```sh
opam install . --deps-only
```

## Run

```sh
make run
```

Starts on `http://localhost:8080`. Configure with environment variables:

| Variable | Default | Description |
|---|---|---|
| `SAKURA_PORT` | `8080` | HTTP port |
| `SAKURA_MAILDIR` | `~/.sakura-mail` | Data directory |

## API

All endpoints except `POST /accounts` require `Authorization: Bearer sk_...`.

```sh
# Register an account (returns id + api_key)
curl -X POST http://localhost:8080/accounts \
  -H 'Content-Type: application/json' \
  -d '{"host":"imap.example.com","port":993,"username":"you","password":"pass","use_tls":true}'

# Sync mail from IMAP server
curl -X POST http://localhost:8080/sync -H 'Authorization: Bearer sk_...'

# List mailboxes
curl http://localhost:8080/mailboxes -H 'Authorization: Bearer sk_...'

# List messages (paginated)
curl 'http://localhost:8080/mailboxes/INBOX/messages?limit=20&offset=0' \
  -H 'Authorization: Bearer sk_...'

# Read a message
curl http://localhost:8080/messages/{id} -H 'Authorization: Bearer sk_...'

# Read raw RFC 5322 content
curl http://localhost:8080/messages/{id}/raw -H 'Authorization: Bearer sk_...'

# Set flags
curl -X PUT http://localhost:8080/messages/{id}/flags \
  -H 'Authorization: Bearer sk_...' \
  -H 'Content-Type: application/json' \
  -d '{"flags":["Seen","Flagged"]}'

# Move message
curl -X POST http://localhost:8080/messages/{id}/move \
  -H 'Authorization: Bearer sk_...' \
  -H 'Content-Type: application/json' \
  -d '{"destination":"Archive"}'

# Delete (move to Trash)
curl -X DELETE http://localhost:8080/messages/{id} -H 'Authorization: Bearer sk_...'

# Create draft
curl -X POST http://localhost:8080/drafts \
  -H 'Authorization: Bearer sk_...' \
  -H 'Content-Type: application/json' \
  -d '{"to":["alice@example.com"],"cc":[],"bcc":[],"subject":"Hello","body":"Hi there","in_reply_to":null}'

# Delete account
curl -X DELETE http://localhost:8080/accounts -H 'Authorization: Bearer sk_...'
```

## Test

```sh
make test          # unit + e2e tests (83 total)
make greenmail-up  # start test IMAP server
make seed          # seed test emails
```

## Project structure

```
bin/              Entry point
lib/
  account/        User registration, API keys, persistence
  api/            Dream HTTP routes
  email/          RFC 5322 parsing (mrmime) and draft generation
  imap/           mbsync CLI wrapper and .mbsyncrc generation
  maildir/        Maildir filesystem operations
  sync/           Background sync scheduler with per-user locking
test/
  seed/           Node.js script to seed test emails via SMTP
```
