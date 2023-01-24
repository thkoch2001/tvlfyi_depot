//! Manual mapping of some data structures in OC/OR corpora.

/// Maps the *names* of OpenRussian word types (the `word_type` field
/// in the `or_words` table) to the *set* of OpenCorpora grammemes
/// commonly attached to lemmata of this type in OC.
///
/// Some word types just don't map over, and are omitted. Many words
/// also have an empty word type.
pub const WORD_TYPES_GRAMMEME_MAP: &'static [(&'static str, &'static [&'static str])] = &[
    ("adjective", &["ADJF"]),
    ("adverb", &["ADVB"]),
    ("noun", &["NOUN"]),
    ("verb", &["INFN"]), // or "VERB" ...
];

/// Maps the *names* of OpenRussian grammemes (the `form_type` fields
/// in the `or_word_forms` table) to the *set* of OpenCorpora
/// grammemes attached to them corresponding lemma in the `oc_lemmas`
/// table.
///
/// This *only* includes grammatical information about the lemma of
/// the word (such as whether it is a verb or other type), but *not*
/// information about the specific instance of the word (such as its
/// gender).
///
/// Correctly corresponding these requires use of all mapping tables.
pub const FORMS_LEMMATA_GRAMMEME_MAP: &'static [(&'static str, &'static [&'static str])] = &[
    ("ru_adj_comparative", &["COMP"]),
    ("ru_adj_superlative", &["ADJF", "Supr"]),
    ("ru_adj_f_acc", &["ADJF"]),
    ("ru_adj_f_dat", &["ADJF"]),
    ("ru_adj_f_gen", &["ADJF"]),
    ("ru_adj_f_inst", &["ADJF"]),
    ("ru_adj_f_nom", &["ADJF"]),
    ("ru_adj_f_prep", &["ADJF"]),
    ("ru_adj_m_acc", &["ADJF"]),
    ("ru_adj_m_dat", &["ADJF"]),
    ("ru_adj_m_gen", &["ADJF"]),
    ("ru_adj_m_inst", &["ADJF"]),
    ("ru_adj_m_nom", &["ADJF"]),
    ("ru_adj_m_prep", &["ADJF"]),
    ("ru_adj_n_acc", &["ADJF"]),
    ("ru_adj_n_dat", &["ADJF"]),
    ("ru_adj_n_gen", &["ADJF"]),
    ("ru_adj_n_inst", &["ADJF"]),
    ("ru_adj_n_nom", &["ADJF"]),
    ("ru_adj_n_prep", &["ADJF"]),
    ("ru_adj_pl_acc", &["ADJF"]),
    ("ru_adj_pl_dat", &["ADJF"]),
    ("ru_adj_pl_gen", &["ADJF"]),
    ("ru_adj_pl_inst", &["ADJF"]),
    ("ru_adj_pl_nom", &["ADJF"]),
    ("ru_adj_pl_prep", &["ADJF"]),
    ("ru_adj_short_f", &["ADJS"]),
    ("ru_adj_short_m", &["ADJS"]),
    ("ru_adj_short_n", &["ADJS"]),
    ("ru_adj_short_pl", &["ADJS"]),
    ("ru_noun_pl_acc", &["NOUN"]),
    ("ru_noun_pl_dat", &["NOUN"]),
    ("ru_noun_pl_gen", &["NOUN"]),
    ("ru_noun_pl_inst", &["NOUN"]),
    ("ru_noun_pl_nom", &["NOUN"]),
    ("ru_noun_pl_prep", &["NOUN"]),
    ("ru_noun_sg_acc", &["NOUN"]),
    ("ru_noun_sg_dat", &["NOUN"]),
    ("ru_noun_sg_gen", &["NOUN"]),
    ("ru_noun_sg_inst", &["NOUN"]),
    ("ru_noun_sg_nom", &["NOUN"]),
    ("ru_noun_sg_prep", &["NOUN"]),
    ("ru_verb_gerund_past", &["GRND"]),
    ("ru_verb_gerund_present", &["GRND"]),
    ("ru_verb_imperative_pl", &["VERB"]),
    ("ru_verb_imperative_sg", &["VERB"]),
    ("ru_verb_past_f", &["VERB"]),
    ("ru_verb_past_m", &["VERB"]),
    ("ru_verb_past_n", &["VERB"]),
    ("ru_verb_past_pl", &["VERB"]),
    ("ru_verb_presfut_pl1", &["VERB"]),
    ("ru_verb_presfut_pl2", &["VERB"]),
    ("ru_verb_presfut_pl3", &["VERB"]),
    ("ru_verb_presfut_sg1", &["VERB"]),
    ("ru_verb_presfut_sg2", &["VERB"]),
    ("ru_verb_presfut_sg3", &["VERB"]),
    (
        "ru_base",
        &[ /* nothing consistent, except often 'Fixd' */ ],
    ),
    ("ru_verb_participle_active_past", &["PRTF", "past", "actv"]),
    (
        "ru_verb_participle_active_present",
        &["PRTF", "pres", "actv"],
    ),
    (
        "ru_verb_participle_passive_past",
        &["PRTF", "past", "passv"],
    ),
    (
        "ru_verb_participle_passive_present",
        &["PRTF", "pres", "passv"],
    ),
];

/// Maps the *names* of OpenRussian grammemes (the `form_type` fields
/// in the `or_word_forms` table) to the *set* of OpenCorpora
/// grammemes attached to them corresponding words in the `oc_words`
/// table.
///
/// This includes grammatical information about the "instance" of the
/// word (such as its gender), but *not* the higher-level type
/// information about its lemma.
///
/// Correctly corresponding these requires use of all mapping tables.
pub const FORMS_WORDS_GRAMMEME_MAP: &'static [(&'static str, &'static [&'static str])] = &[
    ("ru_adj_comparative", &["Cmp2"]),
    ("ru_adj_f_acc", &["femn", "sing", "accs"]),
    ("ru_adj_f_dat", &["femn", "sing", "datv"]),
    ("ru_adj_f_gen", &["femn", "sing", "gent"]),
    ("ru_adj_f_inst", &["femn", "sing", "ablt"]),
    ("ru_adj_f_nom", &["femn", "sing", "nomn"]),
    ("ru_adj_f_prep", &["femn", "sing", "loct"]),
    ("ru_adj_m_acc", &["masc", "sing", "accs"]),
    ("ru_adj_m_dat", &["masc", "sing", "datv"]),
    ("ru_adj_m_gen", &["masc", "sing", "gent"]),
    ("ru_adj_m_inst", &["masc", "sing", "ablt"]),
    ("ru_adj_m_nom", &["masc", "sing", "nomn"]),
    ("ru_adj_m_prep", &["masc", "sing", "loct"]),
    ("ru_adj_n_acc", &["neut", "sing", "accs"]),
    ("ru_adj_n_dat", &["neut", "sing", "datv"]),
    ("ru_adj_n_gen", &["neut", "sing", "gent"]),
    ("ru_adj_n_inst", &["neut", "sing", "ablt"]),
    ("ru_adj_n_nom", &["neut", "sing", "nomn"]),
    ("ru_adj_n_prep", &["neut", "sing", "loct"]),
    ("ru_adj_pl_acc", &["plur", "accs"]),
    ("ru_adj_pl_dat", &["plur", "datv"]),
    ("ru_adj_pl_gen", &["plur", "gent"]),
    ("ru_adj_pl_inst", &["plur", "ablt"]),
    ("ru_adj_pl_nom", &["plur", "nomn"]),
    ("ru_adj_pl_prep", &["plur", "loct"]),
    ("ru_adj_short_f", &["femn", "sing"]),
    ("ru_adj_short_m", &["masc", "sing"]),
    ("ru_adj_short_n", &["neut", "sing"]),
    ("ru_adj_short_pl", &["plur"]),
    ("ru_noun_pl_acc", &["plur", "accs"]),
    ("ru_noun_pl_dat", &["plur", "datv"]),
    ("ru_noun_pl_gen", &["plur", "gent"]),
    ("ru_noun_pl_inst", &["plur", "ablt"]),
    ("ru_noun_pl_nom", &["plur", "nomn"]),
    ("ru_noun_pl_prep", &["plur", "loct"]),
    ("ru_noun_sg_acc", &["sing", "accs"]),
    ("ru_noun_sg_dat", &["sing", "datv"]),
    ("ru_noun_sg_gen", &["sing", "gent"]),
    ("ru_noun_sg_inst", &["sing", "ablt"]),
    ("ru_noun_sg_nom", &["sing", "nomn"]),
    ("ru_noun_sg_prep", &["sing", "loct"]),
    ("ru_verb_gerund_past", &["past", "V-sh"]),
    ("ru_verb_imperative_pl", &["plur", "impr"]),
    ("ru_verb_imperative_sg", &["sing", "impr"]),
    ("ru_verb_past_f", &["femn", "sing", "past"]),
    ("ru_verb_past_m", &["masc", "sing", "past"]),
    ("ru_verb_past_n", &["neut", "sing", "past"]),
    ("ru_verb_past_pl", &["plur", "past"]),
    // these also contain "pres" or "futr", depending on the verb.
    ("ru_verb_presfut_pl1", &["plur", "1per"]),
    ("ru_verb_presfut_pl2", &["plur", "2per"]),
    ("ru_verb_presfut_pl3", &["plur", "3per"]),
    ("ru_verb_presfut_sg1", &["sing", "1per"]),
    ("ru_verb_presfut_sg2", &["sing", "2per"]),
    ("ru_verb_presfut_sg3", &["sing", "3per"]),
    // Unclear items, probably only useful tags on lemmata
    (
        "ru_verb_gerund_present",
        &["pres" /* prob. something missing? */],
    ),
    (
        "ru_adj_superlative",
        &[/* TODO: unclear, random list of grammemes?! */],
    ),
    ("ru_base", &[/* TODO: unclear */]),
    // These have no useful tags in the forms table, only gender &
    // case tagging.
    ("ru_verb_participle_active_past", &[]),
    ("ru_verb_participle_active_present", &[]),
    ("ru_verb_participle_passive_past", &[]),
    ("ru_verb_participle_passive_present", &[]),
];
