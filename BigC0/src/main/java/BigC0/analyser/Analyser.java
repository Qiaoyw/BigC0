package BigC0.analyser;

import BigC0.error.AnalyzeError;
import BigC0.error.CompileError;
import BigC0.error.ErrorCode;
import BigC0.error.ExpectedTokenError;
import BigC0.error.TokenizeError;
import BigC0.instruction.Instruction;
import BigC0.instruction.Operation;
import BigC0.tokenizer.Token;
import BigC0.tokenizer.TokenType;
import BigC0.tokenizer.Tokenizer;
import BigC0.util.Pos;

import java.util.*;

public class Analyser {
    Tokenizer tokenizer;
    ArrayList<Instruction> instructions;

    /**层数**/
    int LEVEL=1;
    /** 当前偷看的 token */
    Token peekedToken = null;

    /**当前函数**/
    Symbol functionNow=new Symbol();

    /** 符号表 */
    List<Symbol> symbolmap=new ArrayList<>();

    /**全局变量表**/
    List<Global> globalmap=new ArrayList<>();

    /**函数输出表**/
    List<Function> functionmap=new ArrayList<>();

    public Analyser(Tokenizer tokenizer) {
        this.tokenizer = tokenizer;
        this.instructions = new ArrayList<>();
    }

    /**不知道是干嘛的*/
    public List<Instruction> analyse() throws CompileError {
        analyseProgram();
        return instructions;
    }

    /**
     * 查看下一个 Token
     *
     * @return
     * @throws TokenizeError
     */
    private Token peek() throws TokenizeError {
        if (peekedToken == null) {
            peekedToken = tokenizer.nextToken();
        }
        return peekedToken;
    }

    /**
     * 获取下一个 Token
     *
     * @return
     * @throws TokenizeError
     */
    private Token next() throws TokenizeError {
        if (peekedToken != null) {
            var token = peekedToken;
            peekedToken = null;
            return token;
        } else {
            return tokenizer.nextToken();
        }
    }

    /**
     * 如果下一个 token 的类型是 tt，则返回 true
     *
     * @param tt
     * @return
     * @throws TokenizeError
     */
    private boolean check(TokenType tt) throws TokenizeError {
        var token = peek();
        return token.getTokenType() == tt;
    }

    /**
     * 如果下一个 token 的类型是 tt，则前进一个 token 并返回这个 token
     *
     * @param tt 类型
     * @return 如果匹配则返回这个 token，否则返回 null
     * @throws TokenizeError
     */
    private Token nextIf(TokenType tt) throws TokenizeError {
        var token = peek();
        if (token.getTokenType() == tt) {
            return next();
        } else {
            return null;
        }
    }

    /**
     * 如果下一个 token 的类型是 tt，则前进一个 token 并返回，否则抛出异常
     *
     * @param tt 类型
     * @return 这个 token
     * @throws CompileError 如果类型不匹配
     */
    private Token expect(TokenType tt) throws CompileError {
        var token = peek();
        if (token.getTokenType() == tt) {
            return next();
        } else {
            throw new ExpectedTokenError(tt, token);
        }
    }


    public void Analyse() throws CompileError{
        analyseProgram();
    }

    private void analyseProgram() throws CompileError {
        //程序结构
        //program -> decl_stmt* function*
        while(check(TokenType.LET_KW)||check(TokenType.CONST_KW)) analyseDeclStmt();
        while(check(TokenType.FN_KW)) analyseFunction();
    }
    /**声明语句*/
    private void analyseDeclStmt() throws CompileError {
        //decl_stmt -> let_decl_stmt | const_decl_stmt
        if(check(TokenType.LET_KW)) analyseLetDeclStmt();
        else if(check(TokenType.CONST_KW)) analyseConstDeclStmt();
        else throw new AnalyzeError(ErrorCode.Break,peekedToken.getEndPos());
    }

    private void analyseLetDeclStmt() throws CompileError {
        //let_decl_stmt -> 'let' IDENT ':' ty ('=' expr)? ';'
        expect(TokenType.LET_KW);
        //记录标识符名字
        Token ident=expect(TokenType.IDENT);
        String name= ident.getValueString();
        expect(TokenType.COLON);

        //类型
        Token ty=analyseTy();
        String type=ty.getValueString();

        //全局变量入表
        if(LEVEL==1) globalmap.add(new Global(false));

        //加符号表
        if(SearchByNameAdd(name)==-1)  symbolmap.add(new Symbol(name,type,false,LEVEL));
        else throw new AnalyzeError(ErrorCode.Break,peekedToken.getEndPos());

        String type2;
        if(check(TokenType.ASSIGN)){
            next();
            //类型是否一致
            type2=analyseExpr();
            if(!type2.equals(type)) throw new AnalyzeError(ErrorCode.Break,peekedToken.getEndPos());
        }
        expect(TokenType.SEMICOLON);


    }

    private void analyseConstDeclStmt() throws CompileError {
        //const_decl_stmt -> 'const' IDENT ':' ty '=' expr ';'
        expect(TokenType.CONST_KW);

        //记录标识符名字
        Token ident=expect(TokenType.IDENT);
        String name= ident.getValueString();

        expect(TokenType.COLON);

        //类型会被记录为const
        Token ty=analyseTy();
        String type=ty.getValueString();

        expect(TokenType.ASSIGN);

        //类型不一致要报错
        String type2=analyseExpr();
        if(!type.equals(type2)) throw new AnalyzeError(ErrorCode.Break,peekedToken.getEndPos());

        expect(TokenType.SEMICOLON);
        //不重复就放进去
        if(SearchByNameAdd(name)==-1) symbolmap.add(new Symbol(name,type,true,LEVEL));
        else throw new AnalyzeError(ErrorCode.Break,peekedToken.getEndPos());
    }

    /**类型系统*/
    private Token analyseTy() throws CompileError {
        //ty -> IDENT 只能是void和int
        Token tt=peek();
        if(tt.getValue().equals("void")||tt.getValue().equals("int")||tt.getValue().equals("double")){
            next();
        }
        //否则抛出异常
        else throw new AnalyzeError(ErrorCode.Break,peekedToken.getEndPos());
        return tt;
    }

    /**函数声明*/
    private void analyseFunction() throws CompileError {
        //function -> 'fn' IDENT '(' function_param_list? ')' '->' ty block_stmt
        expect(TokenType.FN_KW);

        Token tt= expect(TokenType.IDENT);
        String name=tt.getValueString();

        expect(TokenType.L_PAREN);

        //参数列表
        List<Symbol> n=new ArrayList<>();
        if(!check(TokenType.R_PAREN)){
            n=analyseFunctionParamList();
        }
        expect(TokenType.R_PAREN);
        expect(TokenType.ARROW);

        //函数返回值类型
        Token ty=analyseTy();
        String back=ty.getValueString();

        //加入符号表,存当前函数
        Symbol fun=new Symbol(name,"fun",n,LEVEL,back);
        functionNow=fun;
        symbolmap.add(fun);

        analyseBlockStmt();

    }
    private List<Symbol> analyseFunctionParamList() throws CompileError {
        //function_param_list -> function_param (',' function_param)*
        List<Symbol> n=new ArrayList<>();
        //把参数加入参数列表
        n.add(analyseFunctionParam());
        while(check(TokenType.COMMA)){
            next();
            n.add(analyseFunctionParam());
        }
        return n;
    }

    private Symbol analyseFunctionParam() throws CompileError {
        //function_param -> 'const'? IDENT ':' ty
        String type=" ";
        boolean isConst=false;
        if(check(TokenType.CONST_KW)){
            isConst=true;
            next();
        }
        Token ident=expect(TokenType.IDENT);
        String name= ident.getValueString();

        expect(TokenType.COLON);

        Token ty= analyseTy();
        type=ty.getValueString();

        Symbol param=new Symbol(name,type,isConst,LEVEL+1);
        if(SearchByNameAdd(name)==-1) symbolmap.add(param);

        else throw new AnalyzeError(ErrorCode.Break,ident.getStartPos());
        return param;
    }


    /**表达式*/
    private String analyseExpr() throws CompileError {
        //expr ->
        //    | negate_expr
        //    | assign_expr
        //    | call_expr
        //    | literal_expr
        //    | ident_expr
        //    | group_expr
        //     (binary_operator expr||'as' ty)*
        String type="void";
        if(check(TokenType.MINUS)){
            type=analyseNegateExpr();
        }
        else if(check(TokenType.L_PAREN)){
            type=analyseGroupExpr();
        }
        else if(check(TokenType.UINT_LITERAL)||check(TokenType.DOUBLE_LITERAL)||check(TokenType.STRING_LITERAL)){
            type=analyseLiteralExpr();
        }

        else if(check(TokenType.IDENT)){
            //三个以IDENT开头的非终结符
            Token ident= next();
            String name= ident.getValueString();
            int position=SearchByNameExist(name);
            Symbol symbol=new Symbol();
            //记录IDENT的type
            //如果没有找到，看看是不是标准库函数
            if (position==-1){
                symbol=judgeKu(name);
                //不是标准库函数
                if(symbol==null) throw new AnalyzeError(ErrorCode.Break,ident.getStartPos());
            }
            else{
                symbol=symbolmap.get(position);
                type=symbol.getType();
            }
            //函数调用,把type赋值为返回值
            if(check(TokenType.L_PAREN)){
                if(!type.equals("fun")) throw new AnalyzeError(ErrorCode.Break,ident.getStartPos());
                type=analyseCallExpr(symbol);
            }
            //赋值
            else if(check(TokenType.ASSIGN)){
                //常量和函数不能在等号左边
                if(symbol.isConst||(symbol.type.equals("fun"))) throw new AnalyzeError(ErrorCode.Break,ident.getStartPos());
                type=analyseAssignExpr(type);
            }
        }
        while(check(TokenType.AS_KW)||check(TokenType.PLUS)||check(TokenType.MINUS)||check(TokenType.MUL)||check(TokenType.DIV)||check(TokenType.EQ)||check(TokenType.NEQ)||check(TokenType.LT)||check(TokenType.GT)||check(TokenType.LE)||check(TokenType.GE)){
            if(check(TokenType.AS_KW)){
                type=analyseAsExpr();
            }
            else{
                type=analyseOperatorExpr(type);
            }
        }
        return type;
    }
    /**运算符表达式*/
    private void analyseBinaryOperator() throws CompileError {
        //binary_operator -> '+' | '-' | '*' | '/' | '==' | '!=' | '<' | '>' | '<=' | '>='
        if(check(TokenType.PLUS)||check(TokenType.MINUS)||check(TokenType.MUL)||check(TokenType.DIV)||check(TokenType.EQ)||check(TokenType.NEQ)||check(TokenType.LT)||check(TokenType.GT)||check(TokenType.LE)||check(TokenType.GE)){
            next();
        }
        else throw new AnalyzeError(ErrorCode.Break,peekedToken.getStartPos());
    }

    private String analyseOperatorExpr(String typeLeft) throws CompileError {
        //operator_expr -> expr binary_operator expr
        //消除左递归
        analyseBinaryOperator();
        String typeRight=analyseExpr();
        //左右类型一样
        if(typeLeft.equals(typeRight)) return typeLeft;
        else throw new AnalyzeError(ErrorCode.Break,peekedToken.getStartPos());
    }
    /**取反表达式*/
    private String analyseNegateExpr() throws CompileError {
        //negate_expr -> '-' expr
        expect(TokenType.MINUS);
        String type= analyseExpr();
        return type;
    }
    /**赋值表达式*/
    private String analyseAssignExpr(String typeLeft) throws CompileError {
        //assign_expr -> l_expr '=' expr
        //l_expr已经判断过了
        expect(TokenType.ASSIGN);
        String typeRight=analyseExpr();
        //类型是否一样
        if(typeLeft.equals(typeRight)) return "void";
        else throw new AnalyzeError(ErrorCode.Break,peekedToken.getStartPos());
    }
    /**类型转换表达式*/
    private String analyseAsExpr() throws CompileError {
        //as_expr -> expr 'as' ty
        //消除左递归
        expect(TokenType.AS_KW);
        Token ty=analyseTy();
        String type=ty.getValueString();
        return type;
    }
    /**函数调用表达式*/
    //参数
    private void analyseCallParamList(Symbol function) throws CompileError {
        //call_param_list -> expr (',' expr)*
        int position =0;
        String type="int";
        List<Symbol> param=function.param;

        type=analyseExpr();
        //参数类型不同
        if(!param.get(position).type.equals(type)) throw new AnalyzeError(ErrorCode.Break,peekedToken.getStartPos());
        position++;
        while(check(TokenType.COMMA)){
            next();
            type=analyseExpr();
            if(!param.get(position).type.equals(type)) throw new AnalyzeError(ErrorCode.Break,peekedToken.getStartPos());
            position++;
        }
        //参数个数不同
        if(position!=param.size()) throw new AnalyzeError(ErrorCode.Break,peekedToken.getStartPos());
    }
    //调用
    private String analyseCallExpr(Symbol function) throws CompileError {
        //call_expr -> IDENT '(' call_param_list? ')'
        //IDENT判断过了
        //得到函数名字的Symbole

        expect(TokenType.L_PAREN);
        if(!check(TokenType.R_PAREN)){
            analyseCallParamList(function);
        }
        expect(TokenType.R_PAREN);

        return function.getBack();
    }
    /**字面量表达式*/
    private String analyseLiteralExpr() throws CompileError {
        //literal_expr -> UINT_LITERAL | DOUBLE_LITERAL | STRING_LITERAL
        if(check(TokenType.UINT_LITERAL)){
            next();
            return "int";
        }
        else if(check(TokenType.DOUBLE_LITERAL)){
            return "double";
        }
        else if(check(TokenType.STRING_LITERAL)){
            return "string";
        }
        else throw new AnalyzeError(ErrorCode.Break,peekedToken.getStartPos());
    }

    /**标识符表达式*/
    //private void analyseIdentExpr() throws CompileError {
     //   //ident_expr -> IDENT
     //   expect(TokenType.IDENT);
     //
    //}

    /**括号表达式*/
    private String analyseGroupExpr() throws CompileError {
        //group_expr -> '(' expr ')'
        expect(TokenType.L_PAREN);
        String type=analyseExpr();
        expect(TokenType.R_PAREN);
        return type;
    }

    /**语句*/
    private void analyseStmt() throws CompileError {
        //stmt ->
        //      expr_stmt
        //    | decl_stmt *
        //    | if_stmt *
        //    | while_stmt *
        //    | return_stmt *
        //    | block_stmt *
        //    | empty_stmt *
        if(check(TokenType.IF_KW)) analyseIfStmt();
        else if(check(TokenType.WHILE_KW)) analyseWhileStmt();
        else if(check(TokenType.RETURN_KW)) analyseReturnStmt();
        else if(check(TokenType.L_BRACE)) analyseBlockStmt();
        else if(check(TokenType.SEMICOLON)) analyseEmptyStmt();
        else if(check(TokenType.LET_KW)||check(TokenType.CONST_KW)) analyseDeclStmt();
        else analyseExprStmt();
    }
    /**表达式语句*/
    private void analyseExprStmt() throws CompileError {
        //expr_stmt -> expr ';'
        analyseExpr();
        expect(TokenType.SEMICOLON);
    }
    /**控制流语句*/
    private void analyseIfStmt() throws CompileError {
        //if_stmt -> 'if' expr block_stmt ('else' (block_stmt | if_stmt))?
        expect(TokenType.IF_KW);
        analyseExpr();
        analyseBlockStmt();
        if(check(TokenType.ELSE_KW)){
            expect(TokenType.ELSE_KW);
            if(check(TokenType.L_BRACE)) analyseBlockStmt();
            else if(check(TokenType.IF_KW)) analyseIfStmt();
            else throw new AnalyzeError(ErrorCode.Break,peekedToken.getStartPos());
        }
    }
    private void analyseWhileStmt() throws CompileError {
        //while_stmt -> 'while' expr block_stmt
        expect(TokenType.WHILE_KW);
        analyseExpr();
        analyseBlockStmt();
    }
    private void analyseReturnStmt() throws CompileError {
        //return_stmt -> 'return' expr? ';'
        expect(TokenType.RETURN_KW);
        String backType="void";
        if(!check(TokenType.SEMICOLON)){
            backType=analyseExpr();
        }
        //如果返回值不一样，就报错
        if(backType.equals(functionNow.getBack())) throw new AnalyzeError(ErrorCode.Break,peekedToken.getStartPos());
        expect(TokenType.SEMICOLON);
    }
    /**代码块*/
    private void analyseBlockStmt() throws CompileError {
        //进入分程序，改变LEVEL
        LEVEL++;
        //block_stmt -> '{' stmt* '}'
        expect(TokenType.L_BRACE);
        while(!check(TokenType.R_BRACE)) analyseStmt();
        expect(TokenType.R_BRACE);

        //出栈
        outZhan();

        LEVEL--;
    }
    /**空语句*/
    private void analyseEmptyStmt() throws CompileError {
        //empty_stmt -> ';'
        expect(TokenType.SEMICOLON);
    }


   /**按照名字查询符号表**/
   //如果该层没有，返回-1,声明时调用
    public int SearchByNameAdd(String name){
        Symbol n=new Symbol();
        for(int i = symbolmap.size()-1;i >=0;i--) {
            n = symbolmap.get(i);
            if (name.equals(n.name)&&LEVEL==n.getLevel()) return i;
        }
        return -1;
    }

    //如果不存在，用时调用
    public int SearchByNameExist(String name){
        Symbol n=new Symbol();
        for(int i = symbolmap.size()-1;i >=0;i--) {
            n = symbolmap.get(i);
            if (name.equals(n.name)) return i;
        }
        return -1;
    }

    //判断是不是标准库函数，并且返回一个Symbol
    public Symbol judgeKu(String name){
        List<Symbol> param=new ArrayList<>();
        String back="void";
        if(name.equals("getint")||name.equals("getchar")) back="int";
        else if(name.equals("getdouble")) back="double";
        else if(name.equals("putln")) back="void";
        else if(name.equals("putint")){
            param.add(new Symbol("param1","int",false,LEVEL+1));
            back="void";
        }
        else if(name.equals("putdouble")){
            param.add(new Symbol("param1","double",false,LEVEL+1));
            back="void";
        }
        else if(name.equals("putchar")){
            param.add(new Symbol("param1","int",false,LEVEL+1));
            back="void";
        }
        else if(name.equals("putstr")){
            param.add(new Symbol("param1","int",false,LEVEL+1));
            back="void";
        }
        else return null;

        Symbol kuFun=new Symbol(name,"fun",param,LEVEL,back);
        return kuFun;
    }

    //结束一层之后，出栈
    public void outZhan(){
        Symbol n=new Symbol();
        for(int i = symbolmap.size()-1;i >=0;i--) {
            n = symbolmap.get(i);
            if (n.level==LEVEL) symbolmap.remove(i);
            else break;
        }
    }



















}
