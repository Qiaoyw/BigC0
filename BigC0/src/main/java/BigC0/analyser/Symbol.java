package BigC0.analyser;

import java.util.ArrayList;
import java.util.List;

public class Symbol {
    //名字
    String name;
    /**类型
     * 函数，变量类型,int,double,fun
     */
    String type;
    //是否是常量
    boolean isConst;

    //参数列表？
    List<Symbol> param = new ArrayList<>();


    //返回值类型
    String back;
    //大小？
    //层数？
    int level;

    public Symbol(){}
    public Symbol(String name,String type,boolean isConst,int level){
        this.name=name;
        this.type=type;
        this.isConst=isConst;
        this.level=level;
    }

    //函数构造如下
    public Symbol(String name,String type,List<Symbol> param,int level,String back){
        this.name=name;
        this.type=type;
        this.param=param;
        this.level=level;
        this.back=back;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }


    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }


    public List<Symbol> getParam() {
        return param;
    }

    public void setParam(List<Symbol> param) {
        this.param = param;
    }

    public int getLevel() {
        return level;
    }

    public void setLevel(int level) {
        this.level = level;
    }

    public String getBack() {
        return back;
    }

    public void setBack(String back) {
        this.back = back;
    }



}
