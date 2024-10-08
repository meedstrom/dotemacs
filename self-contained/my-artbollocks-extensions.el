;; Extensions to artbollocks-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Martin Edström

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.


;; From upstream (no convenient list is provided)
(defconst artbollocks-default-weasel-words
  '("many" "various" "very" "fairly" "several" "extremely" "exceedingly"
    "quite" "remarkably" "few" "surprisingly" "mostly" "largely" "huge" "tiny"
    "are a number" "is a number" "excellent" "interestingly" "significantly"
    "substantially" "clearly" "vast" "relatively" "completely"))

;; From upstream (no convenient list is provided)
(defconst artbollocks-default-jargon
  '("a priori" "ad hoc" "affirmation" "affirm"
    "affirms" "alterity" "altermodern" "aporia" "aporetic" "appropriates"
    "appropriation" "archetypal" "archetypical" "archetype" "archetypes"
    "autonomous" "autonomy" "baudrillardian" "baudrillarian" "commodification"
    "committed" "commitment" "commonalities" "contemporaneity" "context" "contexts"
    "contextual" "contextualise" "contextualises" "contextualisation"
    "contextialize" "contextializes" "contextualization" "contextuality"
    "convention" "conventional" "conventions" "coterminous" "critique" "cunning"
    "cunningly" "death of the author" "debunk" "debunked" "debunking" "debunks"
    "deconstruct" "deconstruction" "deconstructs" "deleuzian" "desire" "desires"
    "dialectic" "dialectical" "dialectically" "discourse" "discursive" "disrupt"
    "disrupts" "engage" "engagement" "engages" "episteme" "epistemic" "ergo"
    "fetish" "fetishes" "fetishise" "fetishised" "fetishize" "fetishized" "gaze"
    "gender" "gendered" "historicise" "historicisation" "historicize"
    "historicization" "hegemonic" "hegemony" "identity" "identity politics"
    "intensifies" "intensify" "intensifying" "interrogate" "interrogates"
    "interrogation" "intertextual" "intertextuality" "irony" "ironic" "ironical"
    "ironically" "ironisation" "ironization" "ironises" "ironizes" "jouissance"
    "juxtapose" "juxtaposes" "juxtaposition" "lacanian" "lack" "loci" "locus"
    "locuses" "matrix" "mise en abyme" "mocking" "mockingly" "modalities" "modality"
    "myth" "mythologies" "mythology" "myths" "narrative" "narrativisation"
    "narrativization" "narrativity" "nexus" "nodal" "node" "normative" "normativity"
    "notion" "notions" "objective" "objectivity" "objectivities" "objet petit a"
    "ontology" "ontological" "operate" "operates" "otherness" "othering" "paradigm"
    "paradigmatic" "paradigms" "parody" "parodic" "parodies" "physicality"
    "plenitude" "poetics" "popular notions" "position" "post hoc" "post internet"
    "post-internet" "postmodernism" "postmodernist" "postmodernity" "postmodern"
    "practice" "practise" "praxis" "problematic" "problematics" "problematise"
    "problematize" "proposition" "qua" "reading" "readings" "reification" "relation"
    "relational" "relationality" "relations" "representation" "representations"
    "rhizomatic" "rhizome" "simulacra" "simulacral" "simulation" "simulationism"
    "simulationism" "situate" "situated" "situates" "stereotype" "stereotypes"
    "strategy" "strategies" "subjective" "subjectivity" "subjectivities" "subvert"
    "subversion" "subverts" "text" "textual" "textuality" "thinker" "thinkers"
    "trajectory" "transgress" "transgresses" "transgression" "transgressive"
    "unfolding" "undermine" "undermining" "undermines" "work" "works" "wry" "wryly"))

;;; Other English aspects

(defconst my-subs-for-think '("conclude" "define" "expect" "generalize" "hear"
                              "observe" "opine" "postulate" "state" "experience"
                              "remember" "particularize" "deduce" "infer" "induce")
  "Specific evidentials, because \"think\" is overloaded.
See also:
https://lojban.org/publications/cll/cll_v1.1_xhtml-section-chunks/section-evidentials.html")

(defconst my-subs-for-copula '("exist" "remain" "taste" "feel" "smell" "grow" "stay"
                               "turn" "look")
  "See English Prime: https://en.wikipedia.org/wiki/E-Prime")

(defconst my-latinate-native-alist
  '((("adolescence") . ("youth"))
    (("annual") . ("yearly"))
    (("labor") . ("work" "drudge"))
    (("forest") . ("wood" "brush" "grove" "thicket"))
    (("feminine") . ("womanly"))
    (("desire") . ("wish" "will" "yearning" "longing"))
    (("prudent" "intelligent") . ("wise" "clever" "insightful" "enlightened" "keen" "knowledgeable" "shrewd"))
    (("savage" "feral") . ("wild"))
    (("latitude") . ("breadth" "span" "width"))
    (("entire") . ("whole"))
    (("important") . ("weighty" "key"))
    (("lament") . ("mourn" "regret"))
    (("mundane") . ("worldly" "earthly"))
    (("strange") . ("weird" "odd" "queer" "eerie"))
    (("marry") . ("wed"))
    (("marriage") . ("wedlock"))
    (("observe" "supervise") . ("watch" "keep" "behold"))
    (("launder") . ("wash" "cleanse" "rinse" "scrub"))
    (("notice" "alert") . ("warn" "warning" "tip off"))
    (("expect") . ("await"))
    (("salary") . ("wage"))
    (("totally") . ("fully" "wholly" "utterly" "altogether"))
    (("pronounce") . ("utter" "say"))
    (("elevating") . ("uplifting" "raising"))
    (("elevate") . ("uplift" "raise" "lift up"))
    (("support") . ("bolster" "uphold"))
    (("comprehend") . ("understand" "get" "grasp")))
  "See https://en.wikipedia.org/wiki/List_of_Germanic_and_Latinate_equivalents_in_English")

(defconst my-janus-words
  '("ravel" "sanction" "peruse" "fulsome" "oversight")
  "Words that that are antonyms for themselves; broken words.")

(defun my-suggest-substitute ()
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (cond ((member (word-at-point) '("think" "thought"))
           (when-let ((sub (completing-read "Substitute: " my-subs-for-think)))
             (delete-region (car bounds) (cdr bounds))
             (insert sub)))
          ((member (word-at-point) '("am" "are" "be" "is"))
           (when-let ((sub (completing-read "Substitute: " my-subs-for-copula)))
             (delete-region (car bounds) (cdr bounds))
             (insert sub)))
          ((when-let* ((cell (cl-assoc-if (lambda (x) (member (word-at-point) x))
                                          my-latinate-native-alist))
                       (sub (completing-read "Substitute: " (cdr cell))))
             (delete-region (car bounds) (cdr bounds))
             (insert sub))))))

(use-package artbollocks-mode :disabled
  :config
  (setq artbollocks-weasel-words-regex
        (concat "\\b" (regexp-opt
                       (append
                        '(;; Add these:
                          "this is because"
                          "think"
                          "one of the"
                          "should"
                          "just"
                          "sort of"
                          "a lot"
                          "probably"
                          "maybe"
                          "perhaps"
                          "I think"
                          "really"
                          "pretty"
                          "maybe"
                          "nice"
                          "action"
                          "utilize"
                          "leverage")
                        (seq-difference artbollocks-default-weasel-words
                                        '(;; Remove these:
                                          ))))
                "\\b"))

  (setq artbollocks-jargon-regex
        (concat "\\b" (regexp-opt
                       (append
                        '(;; Add these:
                          )
                        (seq-difference artbollocks-default-jargon
                                        '(;; Remove these:
                                          "a priori"
                                          "position"
                                          "matrix"
                                          ))))
                "\\b"))

  (after! artbollocks-mode
    (set-face-attribute 'artbollocks-face () :background 'unspecified)
    (set-face-attribute 'artbollocks-passive-voice-face () :background 'unspecified :foreground "gray")
    (set-face-attribute 'artbollocks-lexical-illusions-face () :background 'unspecified)
    (set-face-attribute 'artbollocks-weasel-words-face () :background 'unspecified)))

