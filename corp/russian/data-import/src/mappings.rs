//! Manual mapping of some data structures in OC/OR corpora.

/// This maps the *names* of OpenRussian grammemes (the set of
/// `form_type` fields in the `word_forms` table) to the *names*
/// of OpenCorpora grammemes.
///
/// The names of the OR grammemes are much easier to understand in
/// general, as the OC ones seem to have strange acronyms in them,
/// however the OC ones are much more structured.
///
/// As these forms map to the word_forms table they lack the forms
/// attached to the lemmata.
pub const FORM_TYPES_GRAMMEMES: &'static [(&'static str, &'static [&'static str])] = &[
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
    ("ru_verb_presfut_pl1", &["plur", "1per", "pres"]),
    ("ru_verb_presfut_pl2", &["plur", "2per", "pres"]),
    ("ru_verb_presfut_pl3", &["plur", "3per", "pres"]),
    ("ru_verb_presfut_sg1", &["sing", "1per", "pres"]),
    ("ru_verb_presfut_sg2", &["sing", "2per", "pres"]),
    ("ru_verb_presfut_sg3", &["sing", "3per", "pres"]),
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
