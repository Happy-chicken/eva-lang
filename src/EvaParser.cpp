#include "../include/EvaParser.h"
namespace syntax {

    std::string Tokenizer::__EOF("$");

    // ------------------------------------------------------------------
    // Lexical rules.

    // clang-format off
    std::array<LexRule, Tokenizer::LEX_RULES_COUNT> Tokenizer::lexRules_ = {{
    {std::regex(R"(^\()"), &_lexRule1},
    {std::regex(R"(^\))"), &_lexRule2},
    {std::regex(R"(^\/\/.*)"), &_lexRule3},
    {std::regex(R"(^\/\*[\s\S]*?\*\/)"), &_lexRule4},
    {std::regex(R"(^\s+)"), &_lexRule5},
    {std::regex(R"(^"[^\"]*")"), &_lexRule6},
    {std::regex(R"(^\d+)"), &_lexRule7},
    {std::regex(R"(^[\w\-+*=!<>/]+)"), &_lexRule8}
    }};
    std::map<TokenizerState, std::vector<size_t>> Tokenizer::lexRulesByStartConditions_ =  {{TokenizerState::INITIAL, {0, 1, 2, 3, 4, 5, 6, 7}}};
    // clang-format on
    void _handler1(yyparse &parser) {
        // Semantic action prologue.
        auto _1 = POP_V();

        auto __ = _1;

        // Semantic action epilogue.
        PUSH_VR();
    }

    void _handler2(yyparse &parser) {
        // Semantic action prologue.
        auto _1 = POP_V();

        auto __ = _1;

        // Semantic action epilogue.
        PUSH_VR();
    }

    void _handler3(yyparse &parser) {
        // Semantic action prologue.
        auto _1 = POP_V();

        auto __ = _1;

        // Semantic action epilogue.
        PUSH_VR();
    }

    void _handler4(yyparse &parser) {
        // Semantic action prologue.
        auto _1 = POP_T();

        auto __ = Exp(std::stoi(_1));

        // Semantic action epilogue.
        PUSH_VR();
    }

    void _handler5(yyparse &parser) {
        // Semantic action prologue.
        auto _1 = POP_T();

        auto __ = Exp(_1);

        // Semantic action epilogue.
        PUSH_VR();
    }

    void _handler6(yyparse &parser) {
        // Semantic action prologue.
        auto _1 = POP_T();

        auto __ = Exp(_1);

        // Semantic action epilogue.
        PUSH_VR();
    }

    void _handler7(yyparse &parser) {
        // Semantic action prologue.
        parser.tokensStack.pop_back();
        auto _2 = POP_V();
        parser.tokensStack.pop_back();

        auto __ = _2;

        // Semantic action epilogue.
        PUSH_VR();
    }

    void _handler8(yyparse &parser) {
        // Semantic action prologue.


        auto __ = Exp(std::vector<Exp>{});

        // Semantic action epilogue.
        PUSH_VR();
    }

    void _handler9(yyparse &parser) {
        // Semantic action prologue.
        auto _2 = POP_V();
        auto _1 = POP_V();

        _1.list.push_back(_2);
        auto __ = _1;

        // Semantic action epilogue.
        PUSH_VR();
    }
    // clang-format off
    std::array<Production, yyparse::PRODUCTIONS_COUNT> yyparse::productions_ = {{{-1, 1, &_handler1},
    {0, 1, &_handler2},
    {0, 1, &_handler3},
    {1, 1, &_handler4},
    {1, 1, &_handler5},
    {1, 1, &_handler6},
    {2, 3, &_handler7},
    {3, 0, &_handler8},
    {3, 2, &_handler9}}};
    // clang-format on

    // ------------------------------------------------------------------
    // Parsing table.

    // clang-format off
    std::array<Row, yyparse::ROWS_COUNT> yyparse::table_ = {
        Row {{0, {TE::Transit, 1}}, {1, {TE::Transit, 2}}, {2, {TE::Transit, 3}}, {4, {TE::Shift, 4}}, {5, {TE::Shift, 5}}, {6, {TE::Shift, 6}}, {7, {TE::Shift, 7}}},
        Row {{9, {TE::Accept, 0}}},
        Row {{4, {TE::Reduce, 1}}, {5, {TE::Reduce, 1}}, {6, {TE::Reduce, 1}}, {7, {TE::Reduce, 1}}, {8, {TE::Reduce, 1}}, {9, {TE::Reduce, 1}}},
        Row {{4, {TE::Reduce, 2}}, {5, {TE::Reduce, 2}}, {6, {TE::Reduce, 2}}, {7, {TE::Reduce, 2}}, {8, {TE::Reduce, 2}}, {9, {TE::Reduce, 2}}},
        Row {{4, {TE::Reduce, 3}}, {5, {TE::Reduce, 3}}, {6, {TE::Reduce, 3}}, {7, {TE::Reduce, 3}}, {8, {TE::Reduce, 3}}, {9, {TE::Reduce, 3}}},
        Row {{4, {TE::Reduce, 4}}, {5, {TE::Reduce, 4}}, {6, {TE::Reduce, 4}}, {7, {TE::Reduce, 4}}, {8, {TE::Reduce, 4}}, {9, {TE::Reduce, 4}}},
        Row {{4, {TE::Reduce, 5}}, {5, {TE::Reduce, 5}}, {6, {TE::Reduce, 5}}, {7, {TE::Reduce, 5}}, {8, {TE::Reduce, 5}}, {9, {TE::Reduce, 5}}},
        Row {{3, {TE::Transit, 8}}, {4, {TE::Reduce, 7}}, {5, {TE::Reduce, 7}}, {6, {TE::Reduce, 7}}, {7, {TE::Reduce, 7}}, {8, {TE::Reduce, 7}}},
        Row {{0, {TE::Transit, 10}}, {1, {TE::Transit, 2}}, {2, {TE::Transit, 3}}, {4, {TE::Shift, 4}}, {5, {TE::Shift, 5}}, {6, {TE::Shift, 6}}, {7, {TE::Shift, 7}}, {8, {TE::Shift, 9}}},
        Row {{4, {TE::Reduce, 6}}, {5, {TE::Reduce, 6}}, {6, {TE::Reduce, 6}}, {7, {TE::Reduce, 6}}, {8, {TE::Reduce, 6}}, {9, {TE::Reduce, 6}}},
        Row {{4, {TE::Reduce, 8}}, {5, {TE::Reduce, 8}}, {6, {TE::Reduce, 8}}, {7, {TE::Reduce, 8}}, {8, {TE::Reduce, 8}}}
    };
    // clang-format on
}// namespace syntax