package BigC0.analyser;

public class Global {
    Boolean is_const;
    int count;
    String items;

    public Global(){}

    public Global(Boolean is_const){
        this.is_const=is_const;
    }

    public Global(Boolean is_const,int count,String items){
        this.is_const=is_const;
        this.count=count;
        this.items=items;
    }

    public Boolean getIs_const() {
        return is_const;
    }

    public void setIs_const(Boolean is_const) {
        this.is_const = is_const;
    }

    public int getCount() {
        return count;
    }

    public void setCount(int count) {
        this.count = count;
    }

    public String getItems() {
        return items;
    }

    public void setItems(String items) {
        this.items = items;
    }
}
