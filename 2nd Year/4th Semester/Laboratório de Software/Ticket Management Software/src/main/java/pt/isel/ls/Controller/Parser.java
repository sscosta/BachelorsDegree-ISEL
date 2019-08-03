package pt.isel.ls.Controller;

import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Exceptions.CommandNotFoundException;
import pt.isel.ls.Exceptions.InvalidParamException;

import javax.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.*;

public class Parser {
    private Map paramMap = new HashMap();
    private String method;
    private String path;
    private String parameters;
    private String header;

    public Parser(String[] args) throws CommandException, UnsupportedEncodingException {
        //validate first arg[0]
        if (args == null || args.length == 0)
            throw new CommandNotFoundException("No command entered.");
        if(args.length<2)
            throw new CommandNotFoundException("Path required");
        else {
            method = args[0].toUpperCase();
            operationParse(method);
        }
        if (args.length > 1) {
            path = args[1];
            pathParse();
        }

        if (args.length>2) {
            if (hasHeader(args[2])) {
                header = args[2];
                addHeaderToMap();
                parameters = hasHeaderAndParams(args)?args[3]:null;
            } else{
                insertDefaultHeader();
                parameters = args[2];
            }
            parameterParse(parameters);
        }else
            insertDefaultHeader();
    }

    public Parser(HttpServletRequest req) throws InvalidParamException, IOException {
        method = req.getMethod();
        path = req.getRequestURI();
        pathParse();
        parameterParse(req.getQueryString());
        if(method.equals("POST"))
            retrieveFromRequestBody(req);
}

    private void insertDefaultHeader() {
        paramMap.put("accept","text/html");
    }

    private boolean hasHeaderAndParams(String [] args) {
        return args.length==4;
    }


    private void operationParse(String command) throws CommandNotFoundException {
        ArrayList<String> registratedOperations = Registrator.getRegistratedOperations();

        if (!registratedOperations.contains(command))
            throw new CommandNotFoundException("Command not found.");
    }

    private void pathParse() throws UnsupportedEncodingException {
        if (path.equals("/")) return;
        String provisoryPath="";
        //if(path.equals("/time"))return;
        String[] aux = path.split("/");
        String last = "";
        String[] pathTreatment = Arrays.copyOfRange(aux, 1, aux.length);
        for (int i = 0; i < pathTreatment.length; i++) {
            String current = pathTreatment[i];
            if (isNumeric(current)) {
                char[]key = new char[last.length()-1];
                last.getChars(0,last.length()-1,key,0);
                int id = Integer.parseInt(current);
                paramMap.put(new String(key)+"ID", id);
                provisoryPath += "/{"+last.charAt(0)+"id}";
            } else if (last.equals("labels")) {
                provisoryPath += "/{label-name}";
                paramMap.put("label-name", URLDecoder.decode(current,"UTF-8"));
            } else
                provisoryPath += "/" + current;
            last = current;
        }
        path=provisoryPath;
    }

    private boolean isNumeric(String str) {
        if (str == null) {
            return false;
        } else {
            int sz = str.length();

            for (int i = 0; i < sz; ++i) {
                if (!Character.isDigit(str.charAt(i))) {
                    return false;
                }
            }

            return true;
        }
    }

    private void parameterParse(String parameters) throws InvalidParamException {
        if (parameters == null) return;
        String[] parameterTreatment = parameters.split("&");
        for (int i = 0; i < parameterTreatment.length; i++) {
            String[] keyValueSplitted = parameterTreatment[i].split("=");
            if(keyIsLabel(keyValueSplitted))
                addToLabelList(keyValueSplitted);
            else if(isToAddLabelToIssue())
                addToLabelList(keyValueSplitted);
            else if (keyValueSplitted.length != 2)
                throw new InvalidParamException("A pair key=value was not entered in parameters");
            else{
                keyValueSplitted[1] = keyValueSplitted[1].replace('+', ' ');
                paramMap.put(keyValueSplitted[0], keyValueSplitted[1]);
            }
        }
    }

    private boolean isToAddLabelToIssue() {
        return path.equals("/projects/{pid}/issues/{iid}/labels");
    }

    private void addToLabelList(String[] keyValueSplitted) {
        List<Pair<String,String>> labels= paramMap.containsKey("labels")?(List<Pair<String,String>>) paramMap.get("labels"):new LinkedList<Pair<String,String>>();
        labels.add(new Pair<String,String> (keyValueSplitted[0],keyValueSplitted[1]));
        paramMap.put("labels",labels);
    }

    private boolean keyIsLabel(String[] keyValueSplitted) {
        return keyValueSplitted[0].equals("label");
    }

    private void addHeaderToMap() throws InvalidParamException {
        String[] split = header.split("\\|");
        if (split.length==2) {
            insertAcceptToMap(split[0]);
            insertFileNameToMap(split[1]);
        }else if(split[0].contains("accept"))
            insertAcceptToMap(split[0]);
        else if(split[0].contains("file-name")) {
            insertFileNameToMap(split[0]);
            insertDefaultHeader();
        }
    }

    private void insertAcceptToMap(String accept) throws InvalidParamException {
        String[] split = accept.split(":");
        if (split[1].equals("text/plain"))
            paramMap.put("accept", split[1]);
        else if (split[1].equals("text/html"))
            paramMap.put("accept", split[1]);
        else
            throw new InvalidParamException(
                    "Please use text/html or text/plain");
    }

    private void insertFileNameToMap(String fileName) throws InvalidParamException {
        String[] split = fileName.split(":");
        if (split.length != 2)
            throw new InvalidParamException(
                    "headers must be in format key:value'");
        if (split[0].equals("file-name"))
            paramMap.put("file-name", split[1]);
        else if(split[0].equals("accept")){
            paramMap.put("accept",split[1]);
        }
            else throw new InvalidParamException(
                    "invalid parameter " + split[0] + " use file-name or accept");
    }

    private void retrieveFromRequestBody(HttpServletRequest req) throws IOException {
        byte[] bytes = new byte[req.getContentLength()];
        req.getInputStream().read(bytes);
        String content = new String(bytes);
        if(content.equals(""))
            return;
        String[] pairs = content.split("&");
        for(String pair : pairs) {
            String[] kvp = pair.split("=");
            try {
                paramMap.put(URLDecoder.decode(kvp[0], "UTF-8"),
                   URLDecoder.decode(kvp[1], "UTF-8"));
            }catch (IndexOutOfBoundsException e){
                return;
            }

        }
    }

    private boolean hasHeader(String parameters) {
        if (parameters.contains("accept") || parameters.contains("file-name"))
            return true;
        return false;
    }

    public Map getParamMap() {
        return paramMap;
    }

    public String getMethod() {
        return method;
    }

    public String getPath() {
        return path;
    }
}
