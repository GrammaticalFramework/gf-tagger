# gf-tagger

This is an experimental part-of-speech tagger for GF. The core is a reimplementation of the Stanford part of speech tagger,
but it is made compatible with GF. You need a PGF file and a corpus and from this you can train a Maximum Entropy model
as described in:

Kristina Toutanova and Christopher D. Manning. 2000. Enriching the Knowledge Sources Used in 
a Maximum Entropy Part-of-Speech Tagger. In Proceedings of the Joint SIGDAT Conference on 
Empirical Methods in Natural Language Processing and Very Large Corpora (EMNLP/VLC-2000), pp. 63-70.

The part of speech tagger will use only the lexical part of the grammar.

The state of this code is still uncertain. For instance part of it might get integrated into the GF runtime. Also to be really
useful, other components are needed as well. For instance the Stanford tagger also uses features for guessing unknown words.
Sentence splitter is also necessary if you want to annotate a whole page. 

Finally, the goal is to be able to annotate free text with the corresponding lexical functions from the grammar.
For that we need not only part-of-speech tagger but also word sense disambiguation.
