package BigC0;

import BigC0.analyser.Analyser;
import BigC0.error.CompileError;
import BigC0.tokenizer.StringIter;
import BigC0.tokenizer.Tokenizer;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class Run {
    public static void main(String[] args) throws FileNotFoundException, CompileError {
        Scanner sc = new Scanner(new File(args[0]));
        StringIter it = new StringIter(sc);
        Tokenizer tn = new Tokenizer(it);
        Analyser analyser = new Analyser(tn);
        analyser.Analyse();
    }
}