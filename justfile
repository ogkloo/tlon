set shell := ["zsh", "-lc"]

default:
  just --list

test:
  cabal test

build:
  cabal build all

agentation-build:
  [ -d node_modules ] || npm install
  npm run build:agentation

cli:
  cabal run tlon

web port="8080":
  npm run build:agentation
  cabal run tlon-web -- --port {{port}}

web-watch port="8080":
  watchexec -r --watch app --watch src --watch static --watch test --watch client --watch package.json --watch package-lock.json --watch tlon.cabal --watch flake.nix --exts hs,cabal,nix,md,js,jsx --ignore dist-newstyle --ignore node_modules -- sh -lc 'npm run build:agentation && cabal run tlon-web -- --port {{port}}'

test-watch:
  watchexec -r --exts hs,cabal,nix --ignore dist-newstyle -- just test

api-state game_id="1" port="8080":
  curl -s http://127.0.0.1:{{port}}/api/games/{{game_id}}/state | jq

api-report game_id="1" port="8080":
  curl -s http://127.0.0.1:{{port}}/api/games/{{game_id}}/report | jq

resolve game_id="1" port="8080":
  curl -s -X POST -D - http://127.0.0.1:{{port}}/games/{{game_id}}/resolve -o /dev/null

check:
  cabal test
  hlint .

fmt:
  fourmolu -i app/Main.hs app/WebMain.hs src/Tlon.hs src/Tlon/Core/Engine.hs src/Tlon/Core/Event.hs src/Tlon/Core/OrderBook.hs src/Tlon/Core/Rng.hs src/Tlon/Core/State.hs src/Tlon/Core/Types.hs src/Tlon/Game/Default/Config.hs src/Tlon/Game/Default/Rules.hs src/Tlon/Game/Default/Setup.hs src/Tlon/Game/Default/View.hs src/Tlon/Web/Api.hs src/Tlon/Web/Server.hs src/Tlon/Web/State.hs src/Tlon/Web/View.hs test/Spec.hs
  cabal-fmt -i tlon.cabal
