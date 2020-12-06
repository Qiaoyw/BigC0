package BigC0.tokenizer;


public enum TokenType {
    /**空*/
    None,
    /**EOF*/
    EOF,
    /** 无符号整数*/
    UINT_LITERAL,
    /**字符串常量*/
    STRING_LITERAL,
    /**标识符*/
    IDENT,

    //扩展字面量
    /**浮点数常量*/
    DOUBLE_LITERAL,
    /**字符常量*/
    CHAR_LITERAL,

    //关键字
    /**fn*/
    FN_KW,
    /**let*/
    LET_KW,
    /**const*/
    CONST_KW,
    /**as*/
    AS_KW,
    /**while*/
    WHILE_KW,
    /**if*/
    IF_KW,
    /**
     * else
     */
    ELSE_KW,
    /**
     * return
     */
    RETURN_KW,

    //扩展关键字
    /**
     * break
     */
    BREAK_KW,
    /**
     * continue
     */
    CONTINUE_KW,

    //运算符
    /**
     * 加号
     */
    PLUS,
    /**
     * 减号
     */
    MINUS,
    /**
     * 乘号
     */
    MUL,
    /**
     * 除号
     */
    DIV,
    /**
     * 等号
     */
    ASSIGN,
    /**
     * ==
     */
    EQ,
    /**
     * '!='
     */
    NEQ,
    /**
     * '<'
     */
    LT,
    /**
     * '>'
     */
    GT,
    /**
     * '>='
     */
    LE,
    /**
     * '<='
     */
    GE,
    /**
     * 左括号
     */
    L_PAREN,
    /**
     * 右括号
     */
    R_PAREN,
    /**
     * {
     */
    L_BRACE,
    /**
     * }
     */
    R_BRACE,
    /**
     * ->
     */
    ARROW,
    /**
     * 逗号
     */
    COMMA,
    /**
     * 冒号
     */
    COLON,
    /**
     * 分号
     */
    SEMICOLON,

    //扩充c0
    /**
     * 注释
     */
    COMMENT;

    @Override
    public String toString(){
        switch (this) {
            case None:
                return "NullToken";
            case IDENT:
                return "Identifier";

            case FN_KW:
                return "fn";
            case LET_KW:
                return "let";
            case CONST_KW:
                return "const";
            case AS_KW:
                return "as";
            case WHILE_KW:
                return "while";
            case IF_KW:
                return "if";
            case ELSE_KW:
                return "else";
            case RETURN_KW:
                return "return";


            case UINT_LITERAL:
                return "UnsignedInteger";
            case STRING_LITERAL:
                return "string";

            case PLUS:
                return "PlusSign";
            case MINUS:
                return "MinusSign";
            case MUL:
                return "MulSign";
            case DIV:
                return "DivSign";
            case ASSIGN:
                return "AssignSign";
            case EQ:
                return "EQSign";
            case NEQ:
                return "NEQSign";
            case LT:
                return "LTSign";
            case GT:
                return "GTSign";
            case LE:
                return "LESign";
            case GE:
                return "GESign";
            case L_PAREN:
                return "LeftBracket";
            case R_PAREN:
                return "RightBracket";
            case L_BRACE:
                return "LeftBigBracket";
            case R_BRACE:
                return "RightBigBracket";
            case ARROW:
                return "ArrowSign";
            case COMMA:
                return "CommaSign";
            case COLON:
                return "ColonSign";
            case SEMICOLON:
                return "SemicolonSign";
            case EOF:
                return "EOF";
            //扩展
            case BREAK_KW:
                return "break";
            case CONTINUE_KW:
                return "continue";
            case DOUBLE_LITERAL:
                return "double";
            case CHAR_LITERAL:
                return "char";
            case COMMENT:
                return "comment";
            default:
                return "InvalidToken";


           }
        }

}