#include QMK_KEYBOARD_H
#include "debug.h"
#include "action_layer.h"
#include "version.h"


#include "keymap_german.h"

#include "keymap_nordic.h"



enum custom_keycodes {
  PLACEHOLDER = SAFE_RANGE, // can always be here
  EPRM,
  VRSN,
  RGB_SLD,

  EX_PIPE, // |>
  THIN_ARROW, // ->
  FAT_ARROW, // =>
};



#define LAMBDA UC(0x03BB)

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {

  [0] = LAYOUT_ergodox(
      KC_EQUAL,       KC_1,           KC_2,   KC_3,   KC_4,   KC_5,   KC_LEFT,
      KC_TAB,         KC_Q,           KC_W,   KC_E,   KC_R,   KC_T,   KC_LALT,
      KC_ESCAPE,      KC_A,           KC_S,   KC_D,   KC_F,   KC_G,
      KC_RSFT,        CTL_T(KC_Z),    KC_X,   KC_C,   KC_V,   KC_B,   KC_TAB,
      LT(1,KC_GRAVE), KC_QUOTE,       LALT(KC_LSHIFT),KC_LEFT,KC_RIGHT,
                                        ALT_T(KC_APPLICATION),      LAMBDA,
                                                                    KC_LBRACKET,
                                        GUI_T(KC_NO), LSFT_T(KC_BSPACE),    KC_COLN,

      KC_MY_COMPUTER, KC_6,   KC_7,   KC_8,       KC_9,       KC_0,               KC_MINUS,
      KC_RALT,      KC_Y,   KC_U,   KC_I,       KC_O,       KC_P,               KC_BSLASH,
                    KC_H,   KC_J,   KC_K,       KC_L,       LT(2,KC_SCOLON),    LT(1,KC_QUOTE),
      KC_MINUS,     KC_N,   KC_M,   KC_COMMA,   KC_DOT,     CTL_T(KC_SLASH),    KC_RSFT,
                    KC_DOWN,KC_UP,  KC_LBRACKET,KC_RBRACKET,MO(1),

      KC_PAUSE,  TG(3),
      KC_RBRACKET,
      KC_COLN,  RSFT_T(KC_ENTER),   KC_SPACE
   ),

  [1] = LAYOUT_ergodox(
      KC_ESCAPE,        KC_F1,          KC_F2,          KC_F3,          KC_F4,      KC_F5,          KC_TRANSPARENT,
      KC_TRANSPARENT,   KC_EXLM,        KC_AT,          KC_LCBR,        KC_RCBR,    KC_PIPE,        KC_RABK,
      KC_TRANSPARENT,   KC_HASH,        KC_DLR,         KC_LPRN,        KC_RPRN,    KC_UNDERSCORE,
      KC_LABK,          KC_PERC,          KC_CIRC,        KC_LBRACKET,    KC_RBRACKET,    KC_TILD,    KC_TRANSPARENT,
      KC_TRANSPARENT,   KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,
                                                        RGB_MOD,  KC_TRANSPARENT,
                                                                  KC_TRANSPARENT,
                                                        RGB_VAD,    RGB_VAI, EX_PIPE,

      KC_TRANSPARENT,   KC_F6,          KC_F7,          KC_F8,          KC_F9,      KC_F10,         KC_F11,
      KC_PGUP,          KC_UP,          KC_7,           KC_8,           KC_9,       KC_ASTR,        KC_F12,
                        KC_DOWN,        KC_4,           KC_5,           KC_6,       KC_PLUS,        KC_TRANSPARENT,
      KC_PGDOWN,        KC_AMPR,        KC_1,           KC_2,           KC_3,       KC_BSLASH,      KC_TRANSPARENT,
                                        KC_TRANSPARENT, KC_DOT,         KC_0,       KC_EQUAL,       KC_TRANSPARENT,
      RGB_TOG,          RGB_SLD,
      THIN_ARROW,
      EX_PIPE,          RGB_HUD,    RGB_HUI
  ),

  [2] = LAYOUT_ergodox(
      KC_SCROLLLOCK,  KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,
      KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_MS_UP,       KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,
      KC_TRANSPARENT, KC_TRANSPARENT, KC_MS_LEFT,     KC_MS_DOWN,     KC_MS_RIGHT,    KC_TRANSPARENT,
      KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,
      KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_MS_BTN1,     KC_MS_BTN2,
                                                       KC_TRANSPARENT,                 KC_TRANSPARENT,
                                                                                       KC_TRANSPARENT,
                                                       KC_MS_BTN1,     KC_MS_BTN2,     KC_TRANSPARENT,

      KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,    KC_TRANSPARENT,      KC_TRANSPARENT,      KC_TRANSPARENT, KC_TRANSPARENT,
      KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,    KC_TRANSPARENT,      KC_TRANSPARENT,      KC_TRANSPARENT, KC_TRANSPARENT,
                      KC_TRANSPARENT, KC_MS_WH_DOWN,     KC_MS_WH_UP,         KC_TRANSPARENT,      KC_TRANSPARENT, KC_MEDIA_PLAY_PAUSE,
      KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,    KC_MEDIA_PREV_TRACK, KC_MEDIA_NEXT_TRACK, KC_TRANSPARENT, KC_TRANSPARENT,
                                      KC_AUDIO_VOL_DOWN, KC_AUDIO_VOL_UP,     KC_AUDIO_MUTE,       KC_TRANSPARENT, KC_TRANSPARENT,
      KC_TRANSPARENT, KC_TRANSPARENT,
      KC_TRANSPARENT,
      KC_TRANSPARENT, KC_TRANSPARENT, KC_WWW_BACK),

  // FPS layout
  [3] = LAYOUT_ergodox(
      KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,
      KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,
      KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,
      KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,
      KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,
                                                      KC_TRANSPARENT,           KC_TRANSPARENT,
                                                                                KC_TRANSPARENT,
                                                      KC_SPACE, KC_TRANSPARENT, KC_TRANSPARENT,

      KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,      KC_TRANSPARENT, KC_TRANSPARENT,
      KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,      KC_TRANSPARENT, KC_TRANSPARENT,
                      KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,      KC_TRANSPARENT, KC_TRANSPARENT,
      KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,      KC_TRANSPARENT, KC_TRANSPARENT,
                                      KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,      KC_TRANSPARENT, KC_TRANSPARENT,
      KC_TRANSPARENT, TG(3),
      KC_TRANSPARENT,
      KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT),
};

const uint16_t PROGMEM fn_actions[] = {
  [1] = ACTION_LAYER_TAP_TOGGLE(1)
};

// leaving this in place for compatibilty with old keymaps cloned and re-compiled.
const macro_t *action_get_macro(keyrecord_t *record, uint8_t id, uint8_t opt)
{
      switch(id) {
        case 0:
        if (record->event.pressed) {
          SEND_STRING (QMK_KEYBOARD "/" QMK_KEYMAP " @ " QMK_VERSION);
        }
        break;
      }
    return MACRO_NONE;
};

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
  switch (keycode) {
    // dynamically generate these.
    case EPRM:
      if (record->event.pressed) {
        eeconfig_init();
      }
      return false;
      break;
    case VRSN:
      if (record->event.pressed) {
        SEND_STRING (QMK_KEYBOARD "/" QMK_KEYMAP " @ " QMK_VERSION);
      }
      return false;
      break;
    case RGB_SLD:
      if (record->event.pressed) {
        rgblight_mode(1);
      }
      return false;
      break;
    case EX_PIPE:
      if (record->event.pressed) {
        SEND_STRING ( "|> " );
      }
      return false;
      break;
    case THIN_ARROW:
      if (record->event.pressed) {
        SEND_STRING ( "-> " );
      }
      return false;
      break;


  }
  return true;
}

void matrix_scan_user(void) {

    uint8_t layer = biton32(layer_state);

    ergodox_board_led_off();
    ergodox_right_led_1_off();
    ergodox_right_led_2_off();
    ergodox_right_led_3_off();
    switch (layer) {
        case 1:
            ergodox_right_led_1_on();
            break;
        case 2:
            ergodox_right_led_2_on();
            break;
        case 3:
            ergodox_right_led_3_on();
            break;
        case 4:
            ergodox_right_led_1_on();
            ergodox_right_led_2_on();
            break;
        case 5:
            ergodox_right_led_1_on();
            ergodox_right_led_3_on();
            break;
        case 6:
            ergodox_right_led_2_on();
            ergodox_right_led_3_on();
            break;
        case 7:
            ergodox_right_led_1_on();
            ergodox_right_led_2_on();
            ergodox_right_led_3_on();
            break;
        default:
            break;
    }

};
