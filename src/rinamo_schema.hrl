
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-define(DEFAULT_SCHEMA_HEADER, "<?xml version=\"1.0\" encoding=\"UTF-8\"?><schema name=\"default\" version=\"1.5\"><fields><field name=\"_version_\" type=\"long\" indexed=\"true\" stored=\"true\" /><field name=\"text\" type=\"text_general\" indexed=\"true\" stored=\"false\" multiValued=\"true\" /><dynamicField name=\"*_i\" type=\"int\" indexed=\"true\" stored=\"true\" /><dynamicField name=\"*_is\" type=\"int\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_l\" type=\"long\" indexed=\"true\" stored=\"true\" /><dynamicField name=\"*_ls\" type=\"long\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_d\" type=\"double\" indexed=\"true\" stored=\"true\" /><dynamicField name=\"*_ds\" type=\"double\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_f\" type=\"float\" indexed=\"true\" stored=\"true\" /><dynamicField name=\"*_fs\" type=\"float\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_s\" type=\"string\" indexed=\"true\" stored=\"true\" /><dynamicField name=\"*_ss\" type=\"string\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_t\" type=\"text_general\" indexed=\"true\" stored=\"false\" /><dynamicField name=\"*_ts\" type=\"text_general\" indexed=\"true\" stored=\"false\" multiValued=\"true\" /><dynamicField name=\"*_tsd\" type=\"text_general\" indexed=\"true\" stored=\"true\" /><dynamicField name=\"*_tssd\" type=\"text_general\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_en\" type=\"text_en\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_ar\" type=\"text_ar\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_bg\" type=\"text_bg\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_ca\" type=\"text_ca\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_cjk\" type=\"text_cjk\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_cz\" type=\"text_cz\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_da\" type=\"text_da\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_de\" type=\"text_de\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_el\" type=\"text_el\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_es\" type=\"text_es\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_eu\" type=\"text_eu\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_fa\" type=\"text_fa\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_fi\" type=\"text_fi\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_fr\" type=\"text_fr\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_ga\" type=\"text_ga\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_gl\" type=\"text_gl\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_hi\" type=\"text_hi\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_hu\" type=\"text_hu\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_hy\" type=\"text_hy\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_id\" type=\"text_id\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_it\" type=\"text_it\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_ja\" type=\"text_ja\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_lv\" type=\"text_lv\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_nl\" type=\"text_nl\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_no\" type=\"text_no\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_pt\" type=\"text_pt\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_ro\" type=\"text_ro\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_ru\" type=\"text_ru\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_sv\" type=\"text_sv\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_th\" type=\"text_th\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_tr\" type=\"text_tr\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_b\" type=\"boolean\" indexed=\"true\" stored=\"true\" /><dynamicField name=\"*_bs\" type=\"boolean\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_dt\" type=\"date\" indexed=\"true\" stored=\"true\" /><dynamicField name=\"*_dts\" type=\"date\" indexed=\"true\" stored=\"true\" multiValued=\"true\" /><dynamicField name=\"*_coordinate\" type=\"tdouble\" indexed=\"true\" stored=\"false\" multiValued=\"false\" /><dynamicField name=\"*_p\" type=\"location\" indexed=\"true\" stored=\"true\" multiValued=\"false\" /><dynamicField name=\"*_ti\" type=\"tint\" indexed=\"true\" stored=\"true\" /><dynamicField name=\"*_tl\" type=\"tlong\" indexed=\"true\" stored=\"true\" /><dynamicField name=\"*_tf\" type=\"tfloat\" indexed=\"true\" stored=\"true\" /><dynamicField name=\"*_td\" type=\"tdouble\" indexed=\"true\" stored=\"true\" /><dynamicField name=\"*_tdt\" type=\"tdate\" indexed=\"true\" stored=\"true\" /><dynamicField name=\"*_ig\" type=\"ignored\" multiValued=\"true\" /><dynamicField name=\"random_*\" type=\"random\" /><dynamicField name=\"*\" type=\"text_general\" indexed=\"true\" stored=\"false\" multiValued=\"true\" /><field name=\"_yz_id\" type=\"_yz_str\" indexed=\"true\" stored=\"true\" required=\"true\" /><field name=\"_yz_err\" type=\"_yz_str\" indexed=\"true\" /><field name=\"_yz_ed\" type=\"_yz_str\" indexed=\"true\" stored=\"true\" /><field name=\"_yz_pn\" type=\"_yz_str\" indexed=\"true\" stored=\"true\" /><field name=\"_yz_fpn\" type=\"_yz_str\" indexed=\"true\" stored=\"true\" /><field name=\"_yz_vtag\" type=\"_yz_str\" indexed=\"true\" stored=\"true\" /><field name=\"_yz_node\" type=\"_yz_str\" indexed=\"true\" stored=\"true\" /><field name=\"_yz_rt\" type=\"_yz_str\" indexed=\"true\" stored=\"true\" /><field name=\"_yz_rk\" type=\"_yz_str\" indexed=\"true\" stored=\"true\" /><field name=\"_yz_rb\" type=\"_yz_str\" indexed=\"true\" stored=\"true\" />").
-define(DEFAULT_SCHEMA_MIDDLE, "</fields><uniqueKey>_yz_id</uniqueKey><types><fieldType name=\"_yz_str\" class=\"solr.StrField\" sortMissingLast=\"true\" /><fieldType name=\"string\" class=\"solr.StrField\" sortMissingLast=\"true\" /><fieldType name=\"boolean\" class=\"solr.BoolField\" sortMissingLast=\"true\" /><fieldType name=\"int\" class=\"solr.TrieIntField\" precisionStep=\"0\" positionIncrementGap=\"0\" /><fieldType name=\"float\" class=\"solr.TrieFloatField\" precisionStep=\"0\" positionIncrementGap=\"0\" /><fieldType name=\"long\" class=\"solr.TrieLongField\" precisionStep=\"0\" positionIncrementGap=\"0\" /><fieldType name=\"double\" class=\"solr.TrieDoubleField\" precisionStep=\"0\" positionIncrementGap=\"0\" /><fieldType name=\"tint\" class=\"solr.TrieIntField\" precisionStep=\"8\" positionIncrementGap=\"0\" /><fieldType name=\"tfloat\" class=\"solr.TrieFloatField\" precisionStep=\"8\" positionIncrementGap=\"0\" /><fieldType name=\"tlong\" class=\"solr.TrieLongField\" precisionStep=\"8\" positionIncrementGap=\"0\" /><fieldType name=\"tdouble\" class=\"solr.TrieDoubleField\" precisionStep=\"8\" positionIncrementGap=\"0\" /><fieldType name=\"date\" class=\"solr.TrieDateField\" precisionStep=\"0\" positionIncrementGap=\"0\" /><fieldType name=\"tdate\" class=\"solr.TrieDateField\" precisionStep=\"6\" positionIncrementGap=\"0\" /><fieldtype name=\"binary\" class=\"solr.BinaryField\" /><fieldType name=\"random\" class=\"solr.RandomSortField\" indexed=\"true\" /><fieldType name=\"text_ws\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.WhitespaceTokenizerFactory\" /></analyzer></fieldType><fieldType name=\"text_general\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer type=\"index\"><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"stopwords.txt\" enablePositionIncrements=\"true\" /><filter class=\"solr.LowerCaseFilterFactory\" /></analyzer><analyzer type=\"query\"><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"stopwords.txt\" enablePositionIncrements=\"true\" /><filter class=\"solr.SynonymFilterFactory\" synonyms=\"synonyms.txt\" ignoreCase=\"true\" expand=\"true\" /><filter class=\"solr.LowerCaseFilterFactory\" /></analyzer></fieldType><fieldType name=\"text_en\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer type=\"index\"><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_en.txt\" enablePositionIncrements=\"true\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.EnglishPossessiveFilterFactory\" /><filter class=\"solr.KeywordMarkerFilterFactory\" protected=\"protwords.txt\" /><filter class=\"solr.PorterStemFilterFactory\" /></analyzer><analyzer type=\"query\"><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.SynonymFilterFactory\" synonyms=\"synonyms.txt\" ignoreCase=\"true\" expand=\"true\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_en.txt\" enablePositionIncrements=\"true\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.EnglishPossessiveFilterFactory\" /><filter class=\"solr.KeywordMarkerFilterFactory\" protected=\"protwords.txt\" /><filter class=\"solr.PorterStemFilterFactory\" /></analyzer></fieldType><fieldType name=\"text_en_splitting\" class=\"solr.TextField\" positionIncrementGap=\"100\" autoGeneratePhraseQueries=\"true\"><analyzer type=\"index\"><tokenizer class=\"solr.WhitespaceTokenizerFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_en.txt\" enablePositionIncrements=\"true\" /><filter class=\"solr.WordDelimiterFilterFactory\" generateWordParts=\"1\" generateNumberParts=\"1\" catenateWords=\"1\" catenateNumbers=\"1\" catenateAll=\"0\" splitOnCaseChange=\"1\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.KeywordMarkerFilterFactory\" protected=\"protwords.txt\" /><filter class=\"solr.PorterStemFilterFactory\" /></analyzer><analyzer type=\"query\"><tokenizer class=\"solr.WhitespaceTokenizerFactory\" /><filter class=\"solr.SynonymFilterFactory\" synonyms=\"synonyms.txt\" ignoreCase=\"true\" expand=\"true\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_en.txt\" enablePositionIncrements=\"true\" /><filter class=\"solr.WordDelimiterFilterFactory\" generateWordParts=\"1\" generateNumberParts=\"1\" catenateWords=\"0\" catenateNumbers=\"0\" catenateAll=\"0\" splitOnCaseChange=\"1\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.KeywordMarkerFilterFactory\" protected=\"protwords.txt\" /><filter class=\"solr.PorterStemFilterFactory\" /></analyzer></fieldType><fieldType name=\"text_en_splitting_tight\" class=\"solr.TextField\" positionIncrementGap=\"100\" autoGeneratePhraseQueries=\"true\"><analyzer><tokenizer class=\"solr.WhitespaceTokenizerFactory\" /><filter class=\"solr.SynonymFilterFactory\" synonyms=\"synonyms.txt\" ignoreCase=\"true\" expand=\"false\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_en.txt\" /><filter class=\"solr.WordDelimiterFilterFactory\" generateWordParts=\"0\" generateNumberParts=\"0\" catenateWords=\"1\" catenateNumbers=\"1\" catenateAll=\"0\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.KeywordMarkerFilterFactory\" protected=\"protwords.txt\" /><filter class=\"solr.EnglishMinimalStemFilterFactory\" /><filter class=\"solr.RemoveDuplicatesTokenFilterFactory\" /></analyzer></fieldType><fieldType name=\"text_general_rev\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer type=\"index\"><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"stopwords.txt\" enablePositionIncrements=\"true\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.ReversedWildcardFilterFactory\" withOriginal=\"true\" maxPosAsterisk=\"3\" maxPosQuestion=\"2\" maxFractionAsterisk=\"0.33\" /></analyzer><analyzer type=\"query\"><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.SynonymFilterFactory\" synonyms=\"synonyms.txt\" ignoreCase=\"true\" expand=\"true\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"stopwords.txt\" enablePositionIncrements=\"true\" /><filter class=\"solr.LowerCaseFilterFactory\" /></analyzer></fieldType><fieldType name=\"alphaOnlySort\" class=\"solr.TextField\" sortMissingLast=\"true\" omitNorms=\"true\"><analyzer><tokenizer class=\"solr.KeywordTokenizerFactory\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.TrimFilterFactory\" /><filter class=\"solr.PatternReplaceFilterFactory\" pattern=\"([^a-z])\" replacement=\"\" replace=\"all\" /></analyzer></fieldType><fieldtype name=\"phonetic\" stored=\"false\" indexed=\"true\" class=\"solr.TextField\"><analyzer><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.DoubleMetaphoneFilterFactory\" inject=\"false\" /></analyzer></fieldtype><fieldtype name=\"payloads\" stored=\"false\" indexed=\"true\" class=\"solr.TextField\"><analyzer><tokenizer class=\"solr.WhitespaceTokenizerFactory\" /><filter class=\"solr.DelimitedPayloadTokenFilterFactory\" encoder=\"float\" /></analyzer></fieldtype><fieldType name=\"lowercase\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.KeywordTokenizerFactory\" /><filter class=\"solr.LowerCaseFilterFactory\" /></analyzer></fieldType><fieldType name=\"text_path\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.PathHierarchyTokenizerFactory\" /></analyzer></fieldType><fieldtype name=\"ignored\" stored=\"false\" indexed=\"false\" multiValued=\"true\" class=\"solr.StrField\" /><fieldType name=\"point\" class=\"solr.PointType\" dimension=\"2\" subFieldSuffix=\"_d\" /><fieldType name=\"location\" class=\"solr.LatLonType\" subFieldSuffix=\"_coordinate\" /><fieldtype name=\"geohash\" class=\"solr.GeoHashField\" /><fieldType name=\"text_ar\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_ar.txt\" enablePositionIncrements=\"true\" /><filter class=\"solr.ArabicNormalizationFilterFactory\" /><filter class=\"solr.ArabicStemFilterFactory\" /></analyzer></fieldType><fieldType name=\"text_bg\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_bg.txt\" enablePositionIncrements=\"true\" /><filter class=\"solr.BulgarianStemFilterFactory\" /></analyzer></fieldType><fieldType name=\"text_ca\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.ElisionFilterFactory\" ignoreCase=\"true\" articles=\"lang/contractions_ca.txt\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_ca.txt\" enablePositionIncrements=\"true\" /><filter class=\"solr.SnowballPorterFilterFactory\" language=\"Catalan\" /></analyzer></fieldType><fieldType name=\"text_cjk\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.CJKWidthFilterFactory\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.CJKBigramFilterFactory\" /></analyzer></fieldType><fieldType name=\"text_cz\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_cz.txt\" enablePositionIncrements=\"true\" /><filter class=\"solr.CzechStemFilterFactory\" /></analyzer></fieldType><fieldType name=\"text_da\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_da.txt\" format=\"snowball\" enablePositionIncrements=\"true\" /><filter class=\"solr.SnowballPorterFilterFactory\" language=\"Danish\" /></analyzer></fieldType><fieldType name=\"text_de\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_de.txt\" format=\"snowball\" enablePositionIncrements=\"true\" /><filter class=\"solr.GermanNormalizationFilterFactory\" /><filter class=\"solr.GermanLightStemFilterFactory\" /></analyzer></fieldType><fieldType name=\"text_el\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.GreekLowerCaseFilterFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"false\" words=\"lang/stopwords_el.txt\" enablePositionIncrements=\"true\" /><filter class=\"solr.GreekStemFilterFactory\" /></analyzer></fieldType><fieldType name=\"text_es\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_es.txt\" format=\"snowball\" enablePositionIncrements=\"true\" /><filter class=\"solr.SpanishLightStemFilterFactory\" /></analyzer></fieldType><fieldType name=\"text_eu\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_eu.txt\" enablePositionIncrements=\"true\" /><filter class=\"solr.SnowballPorterFilterFactory\" language=\"Basque\" /></analyzer></fieldType><fieldType name=\"text_fa\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><charFilter class=\"solr.PersianCharFilterFactory\" /><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.ArabicNormalizationFilterFactory\" /><filter class=\"solr.PersianNormalizationFilterFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_fa.txt\" enablePositionIncrements=\"true\" /></analyzer></fieldType><fieldType name=\"text_fi\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_fi.txt\" format=\"snowball\" enablePositionIncrements=\"true\" /><filter class=\"solr.SnowballPorterFilterFactory\" language=\"Finnish\" /></analyzer></fieldType><fieldType name=\"text_fr\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.ElisionFilterFactory\" ignoreCase=\"true\" articles=\"lang/contractions_fr.txt\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_fr.txt\" format=\"snowball\" enablePositionIncrements=\"true\" /><filter class=\"solr.FrenchLightStemFilterFactory\" /></analyzer></fieldType><fieldType name=\"text_ga\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.ElisionFilterFactory\" ignoreCase=\"true\" articles=\"lang/contractions_ga.txt\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/hyphenations_ga.txt\" enablePositionIncrements=\"false\" /><filter class=\"solr.IrishLowerCaseFilterFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_ga.txt\" enablePositionIncrements=\"true\" /><filter class=\"solr.SnowballPorterFilterFactory\" language=\"Irish\" /></analyzer></fieldType><fieldType name=\"text_gl\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_gl.txt\" enablePositionIncrements=\"true\" /><filter class=\"solr.GalicianStemFilterFactory\" /></analyzer></fieldType><fieldType name=\"text_hi\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.IndicNormalizationFilterFactory\" /><filter class=\"solr.HindiNormalizationFilterFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_hi.txt\" enablePositionIncrements=\"true\" /><filter class=\"solr.HindiStemFilterFactory\" /></analyzer></fieldType><fieldType name=\"text_hu\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_hu.txt\" format=\"snowball\" enablePositionIncrements=\"true\" /><filter class=\"solr.SnowballPorterFilterFactory\" language=\"Hungarian\" /></analyzer></fieldType><fieldType name=\"text_hy\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_hy.txt\" enablePositionIncrements=\"true\" /><filter class=\"solr.SnowballPorterFilterFactory\" language=\"Armenian\" /></analyzer></fieldType><fieldType name=\"text_id\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_id.txt\" enablePositionIncrements=\"true\" /><filter class=\"solr.IndonesianStemFilterFactory\" stemDerivational=\"true\" /></analyzer></fieldType><fieldType name=\"text_it\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.ElisionFilterFactory\" ignoreCase=\"true\" articles=\"lang/contractions_it.txt\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_it.txt\" format=\"snowball\" enablePositionIncrements=\"true\" /><filter class=\"solr.ItalianLightStemFilterFactory\" /></analyzer></fieldType><fieldType name=\"text_ja\" class=\"solr.TextField\" positionIncrementGap=\"100\" autoGeneratePhraseQueries=\"false\"><analyzer><tokenizer class=\"solr.JapaneseTokenizerFactory\" mode=\"search\" /><filter class=\"solr.JapaneseBaseFormFilterFactory\" /><filter class=\"solr.JapanesePartOfSpeechStopFilterFactory\" tags=\"lang/stoptags_ja.txt\" enablePositionIncrements=\"true\" /><filter class=\"solr.CJKWidthFilterFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_ja.txt\" enablePositionIncrements=\"true\" /><filter class=\"solr.JapaneseKatakanaStemFilterFactory\" minimumLength=\"4\" /><filter class=\"solr.LowerCaseFilterFactory\" /></analyzer></fieldType><fieldType name=\"text_lv\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_lv.txt\" enablePositionIncrements=\"true\" /><filter class=\"solr.LatvianStemFilterFactory\" /></analyzer></fieldType><fieldType name=\"text_nl\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_nl.txt\" format=\"snowball\" enablePositionIncrements=\"true\" /><filter class=\"solr.StemmerOverrideFilterFactory\" dictionary=\"lang/stemdict_nl.txt\" ignoreCase=\"false\" /><filter class=\"solr.SnowballPorterFilterFactory\" language=\"Dutch\" /></analyzer></fieldType><fieldType name=\"text_no\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_no.txt\" format=\"snowball\" enablePositionIncrements=\"true\" /><filter class=\"solr.SnowballPorterFilterFactory\" language=\"Norwegian\" /></analyzer></fieldType><fieldType name=\"text_pt\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_pt.txt\" format=\"snowball\" enablePositionIncrements=\"true\" /><filter class=\"solr.PortugueseLightStemFilterFactory\" /></analyzer></fieldType><fieldType name=\"text_ro\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_ro.txt\" enablePositionIncrements=\"true\" /><filter class=\"solr.SnowballPorterFilterFactory\" language=\"Romanian\" /></analyzer></fieldType><fieldType name=\"text_ru\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_ru.txt\" format=\"snowball\" enablePositionIncrements=\"true\" /><filter class=\"solr.SnowballPorterFilterFactory\" language=\"Russian\" /></analyzer></fieldType><fieldType name=\"text_sv\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_sv.txt\" format=\"snowball\" enablePositionIncrements=\"true\" /><filter class=\"solr.SnowballPorterFilterFactory\" language=\"Swedish\" /></analyzer></fieldType><fieldType name=\"text_th\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.LowerCaseFilterFactory\" /><filter class=\"solr.ThaiWordFilterFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"true\" words=\"lang/stopwords_th.txt\" enablePositionIncrements=\"true\" /></analyzer></fieldType><fieldType name=\"text_tr\" class=\"solr.TextField\" positionIncrementGap=\"100\"><analyzer><tokenizer class=\"solr.StandardTokenizerFactory\" /><filter class=\"solr.TurkishLowerCaseFilterFactory\" /><filter class=\"solr.StopFilterFactory\" ignoreCase=\"false\" words=\"lang/stopwords_tr.txt\" enablePositionIncrements=\"true\" /><filter class=\"solr.SnowballPorterFilterFactory\" language=\"Turkish\" /></analyzer></fieldType></types>").
-define(DEFAULT_SCHEMA_FOOTER, "</schema>").
-define(NON_USER_FIELDS, ["_version_","text","_yz_id","_yz_err","_yz_ed","_yz_pn","_yz_fpn","_yz_vtag","_yz_node","_yz_rt","_yz_rk","_yz_rb"]).


-define(TYPE_MAP, [{"S", {"string", false}}, {"N", {"number", false}}, {"B", {"text", false}},
				   {"SS", {"string", true}}, {"NN", {"number", true}}, {"BB", {"text", true}}]).

-ifdef(TEST).
-define(DEFAULT_SCHEMA_XML, ?DEFAULT_SCHEMA_HEADER ++ ?DEFAULT_SCHEMA_MIDDLE ++ ?DEFAULT_SCHEMA_FOOTER).
-define(USER_TEST_FIELDS, "<field name=\"first_name\" type=\"string\" indexed=\"true\" stored=\"false\"/><field name=\"last_name\" type=\"text_general\" indexed=\"true\" stored=\"false\"/><field name=\"description\" type=\"string\" indexed=\"true\" stored=\"false\" multiValued=\"true\"/><field name=\"age\" type=\"tdouble\" indexed=\"true\" stored=\"false\"/><field name=\"location\" type=\"location\" indexed=\"true\" stored=\"false\" multiValued=\"true\"/>").
-define(USER_TEST_MIDDLE, "<copyField source=\"first_name\" dest=\"text\"/><copyField source=\"last_name\" dest=\"text\"/><copyField source=\"description\" dest=\"text\"/>").
-define(USER_TEST_SCHEMA_XML, ?DEFAULT_SCHEMA_HEADER ++ ?USER_TEST_FIELDS ++ ?DEFAULT_SCHEMA_MIDDLE ++ ?USER_TEST_MIDDLE ++ ?DEFAULT_SCHEMA_FOOTER).
-define(USER_SCHEMA_JSON,
    "[{\"name\": \"first_name\", \"type\": \"string\"},
      {\"name\": \"last_name\", \"type\": \"text\"},
      {\"name\": \"description\", \"type\": \"multi_string\"},
      {\"name\": \"age\", \"type\": \"number\"},
      {\"name\": \"location\", \"type\": \"geo\"}]").
-endif.