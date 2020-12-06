package BigC0.analyser;

import BigC0.instruction.Instruction;

import java.util.List;

/**全局函数的表**/
public class Function {
    int id;
    int returnSlots;
    int paramSlots;
    int localSlots;
    List<Instruction> body;

    public Function(int id,int returnSlots,int paramSlots,int localSlots,List<Instruction> body){
        this.id=id;
        this.returnSlots=returnSlots;
        this.paramSlots=paramSlots;
        this.localSlots=localSlots;
        this.body=body;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public int getReturnSlots() {
        return returnSlots;
    }

    public void setReturnSlots(int returnSlots) {
        this.returnSlots = returnSlots;
    }

    public int getParamSlots() {
        return paramSlots;
    }

    public void setParamSlots(int paramSlots) {
        this.paramSlots = paramSlots;
    }

    public int getLocalSlots() {
        return localSlots;
    }

    public void setLocalSlots(int localSlots) {
        this.localSlots = localSlots;
    }

    public List<Instruction> getBody() {
        return body;
    }

    public void setBody(List<Instruction> body) {
        this.body = body;
    }
}
