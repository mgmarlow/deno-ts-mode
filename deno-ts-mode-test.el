(require 'ert)
(require 'deno-ts-mode)

(defvar sample-config
"{
  \"lint\": {
    \"rules\": {
      \"tags\": [\"recommended\"]
    }
  },
  \"fmt\": {
    \"useTabs\": true,
    \"lineWidth\": 80
  },
  \"test\": {
    \"include\": [\"src/\"],
    \"exclude\": [\"src/testdata/\"]
  },
  \"tasks\": {
    \"start\": \"deno run --allow-read main.ts\"
  }
}")

(ert-deftest test-deno-ts--build-config ()
  (let ((config (deno-ts--build-config sample-config)))
    (should (equal (deno-ts-config-tasks config)
                   '((start . "deno run --allow-read main.ts"))))
    (should (equal (deno-ts-config-test config)
                   '((include . ["src/"]) (exclude . ["src/testdata/"]))))
    (should (equal (deno-ts-config-lint config)
                   '((rules (tags . ["recommended"])))))
    (should (equal (deno-ts-config-fmt config)
                   '((useTabs . t) (lineWidth . 80))))))
