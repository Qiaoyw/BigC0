package BigC0.tokenizer;


import BigC0.error.ErrorCode;
import BigC0.error.TokenizeError;

public class Tokenizer {
    private StringIter it;

    public Tokenizer(StringIter it) {
        this.it = it;
    }

    //跳过空白字符
    private void skipSpaceCharacters() {
        while (!it.isEOF() && Character.isWhitespace(it.peekChar())) {
            it.nextChar();
        }
    }
    /**
     * 获取下一个 Token
     *
     * @return
     * @throws TokenizeError 如果解析有异常则抛出
     */
    public Token nextToken() throws TokenizeError {
        it.readAll();
        // 跳过之前的所有空白字符
        skipSpaceCharacters();

        if (it.isEOF()) {
            return new Token(TokenType.EOF, "", it.currentPos(), it.currentPos());
        }

        char peek = it.peekChar();
        /**无符号整数  浮点数*/
        if (Character.isDigit(peek)) {
            return lexUIntOrDouble();
        }
        /**关键字  标识符*/
        else if (Character.isAlphabetic(peek)) {
            return lexIdentOrKeyword();
        }
        /**标识符*/
        else if (peek=='_') {
            return lexIdent();
        }
        /**字符串常量*/
        else if (peek=='"') {
            return lexString();
        }
        /**字符常量*/
        else if (peek=='\'') {
            return lexString();
        }
        /**运算符号   注释*/
        else {
            return lexOtherOrUnknown();
        }
    }

    /**无符号整数   浮点数*/
    //目前只有无符号整数
    private Token lexUIntOrDouble() throws TokenizeError {
        String shu="";
        int Int=0;
        while(Character.isDigit(it.peekChar())){
            shu=shu+it.nextChar();
        }
        //浮点数在这儿判断
        Int=Integer.parseInt(shu);

        // 解析存储的字符串为无符号整数
        // 解析成功则返回无符号整数类型的token，否则返回编译错误
        //
        // Token 的 Value 应填写数字的值
        return new Token(TokenType.UINT_LITERAL,Int, it.previousPos(), it.currentPos());
    }

    /**关键字  标识符*/
    private Token lexIdentOrKeyword() throws TokenizeError {
        String word="";
        while(Character.isLetterOrDigit(it.peekChar())){
            word=word+it.nextChar();
        }
        // 尝试将存储的字符串解释为关键字
        // -- 如果是关键字，则返回关键字类型的 token
        // -- 否则，返回标识符
        //
        // Token 的 Value 应填写标识符或关键字的字符串
        if(word.equals("fn"))  return new Token(TokenType.FN_KW,"fn", it.previousPos(), it.currentPos());
        else if(word.equals("let"))  return new Token(TokenType.LET_KW,"let", it.previousPos(), it.currentPos());
        else if(word.equals("const"))  return new Token(TokenType.CONST_KW,"const", it.previousPos(), it.currentPos());
        else if(word.equals("as"))  return new Token(TokenType.AS_KW,"as", it.previousPos(), it.currentPos());
        else if(word.equals("while"))  return new Token(TokenType.WHILE_KW,"while", it.previousPos(), it.currentPos());
        else if(word.equals("if"))  return new Token(TokenType.IF_KW,"if", it.previousPos(), it.currentPos());
        else if(word.equals("else"))  return new Token(TokenType.ELSE_KW,"else", it.previousPos(), it.currentPos());
        else if(word.equals("return"))  return new Token(TokenType.RETURN_KW,"return", it.previousPos(), it.currentPos());
        /**扩展c0*/
        else if(word.equals("break"))  return new Token(TokenType.BREAK_KW,"break", it.previousPos(), it.currentPos());
        else if(word.equals("continue"))  return new Token(TokenType.CONTINUE_KW,"continue", it.previousPos(), it.currentPos());
        else return new Token(TokenType.IDENT,word, it.previousPos(), it.currentPos());
    }

    /**标识符*/
    private Token lexIdent() throws TokenizeError {
        String word="";
        while(Character.isLetterOrDigit(it.peekChar())){
            word=word+it.nextChar();
        }
        return new Token(TokenType.IDENT,word, it.previousPos(), it.currentPos());
    }


    /**运算符号   注释 */
    private Token lexOtherOrUnknown() throws TokenizeError {
        switch (it.nextChar()) {
            //无歧义运算符
            case '+':
                return new Token(TokenType.PLUS, '+', it.previousPos(), it.currentPos());
            case '*':
                return new Token(TokenType.MUL, '*', it.previousPos(), it.currentPos());
            case '/':
                return new Token(TokenType.DIV, '/', it.previousPos(), it.currentPos());
            //在这里判断是否是注释
            case '(':
                return new Token(TokenType.L_PAREN, '(', it.previousPos(), it.currentPos());
            case ')':
                return new Token(TokenType.R_PAREN, ')', it.previousPos(), it.currentPos());
            case '{':
                return new Token(TokenType.L_BRACE, '{', it.previousPos(), it.currentPos());
            case '}':
                return new Token(TokenType.R_BRACE, '}', it.previousPos(), it.currentPos());
            case ',':
                return new Token(TokenType.COMMA, ',', it.previousPos(), it.currentPos());
            case ':':
                return new Token(TokenType.COLON, ':', it.previousPos(), it.currentPos());
            case ';':
                return new Token(TokenType.SEMICOLON, ';', it.previousPos(), it.currentPos());

            //减号  ->
            case '-':
                if(it.peekChar()=='>'){
                    it.nextChar();
                    return new Token(TokenType.ARROW, "->", it.previousPos(), it.currentPos());
                }
                return new Token(TokenType.MINUS, '-', it.previousPos(), it.currentPos());

            //赋值  等于
            case '=':
                if(it.peekChar()=='='){
                    it.nextChar();
                    return new Token(TokenType.EQ, "==", it.previousPos(), it.currentPos());
                }
                return new Token(TokenType.ASSIGN, '=', it.previousPos(), it.currentPos());

            //不等于
            case '!':
                if(it.peekChar()=='='){
                    it.nextChar();
                    return new Token(TokenType.NEQ, "!=", it.previousPos(), it.currentPos());
                }
                throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());

            //小于  小于等于
            case '<':
                if(it.peekChar()=='='){
                    it.nextChar();
                    return new Token(TokenType.LE, "<=", it.previousPos(), it.currentPos());
                }
                return new Token(TokenType.LT, '<', it.previousPos(), it.currentPos());
            //大于  大于等于
            case '>':
                if(it.peekChar()=='='){
                    it.nextChar();
                    return new Token(TokenType.GE, ">=", it.previousPos(), it.currentPos());
                }
                return new Token(TokenType.GT, '>', it.previousPos(), it.currentPos());

            default:
                throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
        }
    }

    /**字符串常量*/
    private Token lexString() throws TokenizeError {
        String chuan="\"";
        it.nextChar();
        while(true){
            if(it.peekChar()!='"') chuan=chuan+it.nextChar();
            //双引号可能被转义
            else{
                if(chuan.charAt(chuan.length()-1)=='\\'){
                    if(chuan.length()>1&&chuan.charAt(chuan.length()-2)=='\\'){
                        chuan=chuan+it.nextChar();
                        break;
                    }
                    else chuan=chuan+it.nextChar();
                }
                else{
                    chuan=chuan+it.nextChar();
                    break;
                }
            }
        }
        return new Token(TokenType.STRING_LITERAL, chuan, it.previousPos(), it.currentPos());
    }


}
