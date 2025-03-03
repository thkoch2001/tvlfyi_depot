/**
 * Copyright 2022 Charly Delay <charly@codesink.dev> (@0xcharly)
 * Copyright 2023 casuanoob <casuanoob@hotmail.com> (@casuanoob)
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include QMK_KEYBOARD_H

enum dilemma_keymap_layers {
    LAYER_BASE = 0,
    LAYER_NAVIGATION,
    LAYER_MOUSE,
    LAYER_MEDIA,
    LAYER_NUMERAL,
    LAYER_SYMBOLS,
    LAYER_FUNCTION,
};

// Automatically enable sniping-mode on the pointer layer.
#define DILEMMA_AUTO_SNIPING_ON_LAYER LAYER_MOUSE
#define ESC_MED LT(LAYER_MEDIA, KC_ESC)
#define SPC_NAV LT(LAYER_NAVIGATION, KC_SPC)
#define TAB_MOU LT(LAYER_MOUSE, KC_TAB)
#define ENT_SYM LT(LAYER_SYMBOLS, KC_ENT)
#define BSP_NUM LT(LAYER_NUMERAL, KC_BSPC)
#define DEL_FUN LT(LAYER_FUNCTION, KC_DEL)

#ifndef POINTING_DEVICE_ENABLE
#    define DRGSCRL KC_NO
#    define DPI_MOD KC_NO
#    define S_D_MOD KC_NO
#    define SNIPING KC_NO
#endif // !POINTING_DEVICE_ENABLE

// clang-format off
/** \brief COLEMAK-DH layout (3 rows, 10 columns). */
#define LAYOUT_LAYER_BASE                                                                     \
       KC_Q,    KC_W,    KC_F,    KC_P,    KC_B,    KC_J,    KC_L,    KC_U,    KC_Y, KC_QUOT, \
       KC_A,    KC_R,    KC_S,    KC_T,    KC_G,    KC_M,    KC_N,    KC_E,    KC_I,    KC_O, \
       KC_Z,    KC_X,    KC_C,    KC_D,    KC_V,    KC_K,    KC_H, KC_COMMA, KC_DOT, KC_SLSH, \
                      ESC_MED, SPC_NAV, TAB_MOU, ENT_SYM, BSP_NUM, DEL_FUN

/** Convenience row shorthands. */
#define _______________DEAD_HALF_ROW_______________ XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX
#define ______________HOME_ROW_GACS_L______________ KC_LGUI, KC_LALT, KC_LCTL, KC_LSFT, XXXXXXX
#define ______________HOME_ROW_GACS_R______________ XXXXXXX, KC_LSFT, KC_LCTL, KC_LALT, KC_LGUI

/*
 * Layers used on the Dilemma.
 *
 * These layers started off heavily inspired by the Miryoku layout, but trimmed
 * down and tailored for a stock experience that is meant to be fundation for
 * further personalization.
 *
 * See https://github.com/manna-harbour/miryoku for the original layout.
 */

/**
 * \brief Navigation layer.
 *
 * Primary left-hand layer (left home thumb) is navigation and editing. Cursor
 * keys are on the home position, line and page movement below, clipboard
 * above, caps lock and insert on the inner column. Thumb keys are duplicated
 * from the base layer to avoid having to layer change mid edit and to enable
 * auto-repeat.
*/
#define LAYOUT_LAYER_NAVIGATION                                                               \
    _______________DEAD_HALF_ROW_______________, KC_AGAIN,LCTL(KC_V), LCTL(KC_C),  KC_CUT, KC_UNDO, \
    ______________HOME_ROW_GACS_L______________, KC_CAPS, KC_LEFT,    KC_DOWN,   KC_UP, KC_RGHT, \
    _______________DEAD_HALF_ROW_______________,  KC_INS, KC_HOME,    KC_PGDN, KC_PGUP,  KC_END, \
                      XXXXXXX, _______, XXXXXXX,  KC_ENT, KC_BSPC,    KC_DEL

/**
 * \brief Mouse layer
 *
 * Secondary left-hand layer is mouse emulation. Mouse movement mirrors cursor
 * navigation on home and wheel mirrors line / page movement below. Mouse
 * buttons are on the thumbs. Left, right, and middle mouse buttons are on the
 * primary, secondary, and tertiary thumb keys, respectively. Mouse movement,
 * click, and drag, with modifiers, can be performed from the home position.
 * Clipboard keys are duplicated from the Nav layer.
*/
#define LAYOUT_LAYER_MOUSE                                                                    \
    _______________DEAD_HALF_ROW_______________, KC_AGAIN,KC_PSTE, KC_COPY,  KC_CUT, KC_UNDO, \
    ______________HOME_ROW_GACS_L______________, _______, KC_MS_L, KC_MS_D, KC_MS_U, KC_MS_R, \
    _______________DEAD_HALF_ROW_______________, _______, KC_WH_L, KC_WH_D, KC_WH_U, KC_WH_R, \
                      XXXXXXX, XXXXXXX, _______, KC_BTN2, KC_BTN1, KC_BTN3

/**
 * \brief Media layer
 *
 * Tertiary left-hand layer is media control, with volume up / volume down and
 * next / prev mirroring the navigation keys. Pause, stop and mute are on the
 * primary, secondary, and tertiary thumbs, respectively.
 *
 * Keyboard hardware controls are also present, and depend on hardware and
 * firmware support.
 *
 * RGB control is on the top row. RGB Toggle is on the inner index column key.
 * Combine with Shift for RGB Off. RGB Mode, RGB Hue, RGB Saturation, and RGB
 * Value are on index, middle, ring, and pinkie column keys, respectively.
 * Tapping will increase the corresponding value. Combine with Shift to
 * decrease.
*/
#define LAYOUT_LAYER_MEDIA                                                                    \
    _______________DEAD_HALF_ROW_______________, RGB_TOG, RGB_MOD, RGB_HUI, RGB_SAI, RGB_VAI, \
    ______________HOME_ROW_GACS_L______________, _______, KC_MPRV, KC_VOLD, KC_VOLU, KC_MNXT, \
    _______________DEAD_HALF_ROW_______________, _______, _______, _______, _______, _______, \
                      _______, XXXXXXX, XXXXXXX, KC_MSTP, KC_MPLY, KC_MUTE

/**
 * \brief Numeral layout.
 *
 * Primary right-hand layer (right home thumb) is numerals and symbols. Numerals
 * are in the standard numpad locations with symbols in the remaining positions.
 */
#define LAYOUT_LAYER_NUMERAL                                                                  \
    KC_LBRC,    KC_7,    KC_8,    KC_9, KC_RBRC, _______________DEAD_HALF_ROW_______________, \
    KC_SCLN,    KC_4,    KC_5,    KC_6,  KC_EQL, ______________HOME_ROW_GACS_R______________, \
    KC_GRAVE,   KC_1,    KC_2,    KC_3, KC_BSLS, _______________DEAD_HALF_ROW_______________, \
                       KC_DOT, KC_0, KC_MINS, XXXXXXX, _______, XXXXXXX

/**
 * \brief Symbols layer.
 *
 * Secondary right-hand layer has shifted symbols in the same locations to reduce
 * chording when using mods with shifted symbols. `KC_LPRN` is duplicated next to
 * `KC_RPRN`.
 */
#define LAYOUT_LAYER_SYMBOLS                                                                  \
    KC_LCBR, KC_AMPR, KC_ASTR, KC_LPRN, KC_RCBR, _______________DEAD_HALF_ROW_______________, \
    KC_COLN,  KC_DLR, KC_PERC, KC_CIRC, KC_PLUS, ______________HOME_ROW_GACS_R______________, \
    KC_TILD, KC_EXLM,   KC_AT, KC_HASH, KC_PIPE, _______________DEAD_HALF_ROW_______________, \
                      KC_LPRN, KC_RPRN, KC_UNDS, _______, XXXXXXX, XXXXXXX

/**
 * \brief Function layer.
 *
 * Tertiary right-hand layer has function keys mirroring the numerals on the
 * primary layer with system keys on the inner column. App is on the tertiary
 * thumb key and other thumb keys are duplicated from the base layer to enable
 * auto-repeat.
 */
#define LAYOUT_LAYER_FUNCTION                                                                 \
     KC_F12,   KC_F7,   KC_F8,   KC_F9, KC_PSCR, _______________DEAD_HALF_ROW_______________, \
     KC_F11,   KC_F4,   KC_F5,   KC_F6, KC_SCRL, ______________HOME_ROW_GACS_R______________, \
     KC_F10,   KC_F1,   KC_F2,   KC_F3, KC_PAUS, _______________DEAD_HALF_ROW_______________, \
                      KC_APP, KC_SPC, KC_TAB, XXXXXXX, XXXXXXX, _______

/**
 * \brief Add Home Row mod to a layout.
 *
 * Expects a 10-key per row layout.  Adds support for GACS (Gui, Alt, Ctl, Shift)
 * home row.  The layout passed in parameter must contain at least 20 keycodes.
 *
 * This is meant to be used with `LAYER_BASE` defined above, eg.:
 *
 *     HOME_ROW_MOD_GACS(LAYER_BASE)
 */
#define _HOME_ROW_MOD_GACS(                                            \
    L00, L01, L02, L03, L04, R05, R06, R07, R08, R09,                  \
    L10, L11, L12, L13, L14, R15, R16, R17, R18, R19,                  \
    ...)                                                               \
              L00,         L01,         L02,         L03,         L04, \
              R05,         R06,         R07,         R08,         R09, \
      LGUI_T(L10), LALT_T(L11), LCTL_T(L12), LSFT_T(L13), RALT_T(L14), \
      RALT_T(R15), RSFT_T(R16), RCTL_T(R17), LALT_T(R18), RGUI_T(R19), \
      __VA_ARGS__
#define HOME_ROW_MOD_GACS(...) _HOME_ROW_MOD_GACS(__VA_ARGS__)


#define LAYOUT_wrapper(...) LAYOUT_split_3x5_3(__VA_ARGS__)

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
  [LAYER_BASE] = LAYOUT_wrapper(
    HOME_ROW_MOD_GACS(LAYOUT_LAYER_BASE)
  ),
  [LAYER_NAVIGATION] = LAYOUT_wrapper(LAYOUT_LAYER_NAVIGATION),
  [LAYER_MOUSE] = LAYOUT_wrapper(LAYOUT_LAYER_MOUSE),
  [LAYER_MEDIA] = LAYOUT_wrapper(LAYOUT_LAYER_MEDIA),
  [LAYER_NUMERAL] = LAYOUT_wrapper(LAYOUT_LAYER_NUMERAL),
  [LAYER_SYMBOLS] = LAYOUT_wrapper(LAYOUT_LAYER_SYMBOLS),
  [LAYER_FUNCTION] = LAYOUT_wrapper(LAYOUT_LAYER_FUNCTION),
};
// clang-format on

#ifdef POINTING_DEVICE_ENABLE
#    ifdef DILEMMA_AUTO_SNIPING_ON_LAYER
layer_state_t layer_state_set_user(layer_state_t state) {
    dilemma_set_pointer_sniping_enabled(layer_state_cmp(state, DILEMMA_AUTO_SNIPING_ON_LAYER));
    return state;
}
#    endif // DILEMMA_AUTO_SNIPING_ON_LAYER
#endif     // POINTING_DEVICE_ENABLE

#ifdef ENCODER_MAP_ENABLE
// clang-format off
const uint16_t PROGMEM encoder_map[][NUM_ENCODERS][2] = {
    [LAYER_BASE]       = {ENCODER_CCW_CW(KC_WH_D, KC_WH_U),  ENCODER_CCW_CW(KC_VOLD, KC_VOLU)},
    [LAYER_NAVIGATION] = {ENCODER_CCW_CW(KC_PGDN, KC_PGUP),  ENCODER_CCW_CW(KC_VOLU, KC_VOLD)},
    [LAYER_MOUSE]      = {ENCODER_CCW_CW(RGB_HUD, RGB_HUI),  ENCODER_CCW_CW(RGB_SAD, RGB_SAI)},
    [LAYER_MEDIA]      = {ENCODER_CCW_CW(KC_PGDN, KC_PGUP),  ENCODER_CCW_CW(KC_VOLU, KC_VOLD)},
    [LAYER_NUMERAL]    = {ENCODER_CCW_CW(RGB_VAD, RGB_VAI),  ENCODER_CCW_CW(RGB_SPD, RGB_SPI)},
    [LAYER_SYMBOLS]    = {ENCODER_CCW_CW(RGB_RMOD, RGB_MOD), ENCODER_CCW_CW(KC_LEFT, KC_RGHT)},
    [LAYER_FUNCTION]   = {ENCODER_CCW_CW(KC_DOWN, KC_UP),    ENCODER_CCW_CW(KC_LEFT, KC_RGHT)},
};
// clang-format on
#endif // ENCODER_MAP_ENABLE
