package pt.isel.ls.Model;


import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.LinkedList;
import java.util.Objects;

public class Issue {

    private LinkedList<Comment> commentList = new LinkedList<>();
    private LinkedList<Label> labelList = new LinkedList<>();
    public int ID;
    public String name;
    public String description;
    public java.sql.Timestamp creationDate;
    public java.sql.Timestamp closedDate;

    public String projectName;
    public String statusDesc; //STRING?
    public java.sql.Timestamp updated;

    public Issue(int ID, String name, String description, Timestamp creationDate, Timestamp closedDate, String projectName, String statusDesc, Timestamp updated) {
        this.ID = ID;
        this.name = name;
        this.description = description;
        this.creationDate = creationDate;
        this.closedDate = closedDate;
        this.projectName = projectName;
        this.statusDesc = statusDesc;
        this.updated = updated;
    }

    public void setComments(LinkedList<Comment> commentLinkedlist) {
        commentList=commentLinkedlist;
    }

    public void setLabels(LinkedList<Label> labelsInIssue) { labelList=labelsInIssue;}

    public LinkedList<Comment> getComments() {
        return commentList;
    }
    public LinkedList<Label> getLabels(){
        return labelList;
    }

    @Override
    public String toString() {
        String commentParse = "";
        String labelParse= "";
        if(commentList!=null) {
            String newline = "";
            for (Comment cmt : commentList){
                commentParse+=newline;
                newline="\n";
                commentParse += cmt.toString();
            }
        }
        if(labelList!=null){
            String newline = "";
            for (Label lbl : labelList) {
                labelParse += newline;
                newline = "\n";
                labelParse += lbl.toString();
            }
        }
        SimpleDateFormat dateFormat = new SimpleDateFormat("dd-MM-yyyy HH:mm:ss");
        String upTimeStamp = dateFormat.format(updated);
        String creationDateStamp = dateFormat.format(creationDate);
        String closeDateStamp =  dateFormat.format(closedDate);

        String closeDateFormat = "\tClose Date -> -\n";
        if(closedDate!=null)
            closeDateFormat = "\tClose Date -> "+closeDateStamp+'\n';

        String updatedDateFormat = "\tupdated -> -\n";
        if(updated!=null)
            updatedDateFormat = "\tupdated -> "+upTimeStamp+'\n';

        return "Issue:\n" +
                "\tID -> " + ID +'\n'+
                "\tname -> " + name +'\n'+
                "\tdescription -> " + description +'\n'+
                "\tcreation Date -> " + creationDateStamp +'\n'+
                closeDateFormat +
                "\tproject Name -> " + projectName +'\n'+
                "\tstatus Description -> " + statusDesc +'\n'+
                updatedDateFormat + "\n"+
                commentParse +'\n'+
                labelParse;
    }

    public void setID(int ID) {
        this.ID = ID;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Issue issue = (Issue) o;
        return ID == issue.ID &&
                Objects.equals(name, issue.name) &&
                Objects.equals(description, issue.description) &&
                Objects.equals(creationDate, issue.creationDate) &&
                Objects.equals(closedDate, issue.closedDate) &&
                Objects.equals(projectName, issue.projectName) &&
                Objects.equals(statusDesc, issue.statusDesc) &&
                Objects.equals(updated,issue.updated);
    }

    @Override
    public int hashCode() {
        return Objects.hash(ID, name, description, creationDate, closedDate, projectName,  statusDesc,updated);
    }



    public static final class IssueBuilder {
        private int ID;
        private String name;
        private String description;
        private Timestamp creationDate;
        private Timestamp closedDate;
        private Timestamp updated;
        private String projectName;
        private String statusDesc;

        public IssueBuilder() {
        }

        public static IssueBuilder aIssue() {
            return new IssueBuilder();
        }

        public IssueBuilder withID(int ID) {
            this.ID = ID;
            return this;
        }

        public IssueBuilder withName(String name) {
            this.name = name;
            return this;
        }

        public IssueBuilder withDescription(String description) {
            this.description = description;
            return this;
        }

        public IssueBuilder withCreationDate(Timestamp creationDate){
            this.creationDate = creationDate;
            return this;
        }

        public IssueBuilder withClosedDate(Timestamp closedDate){
            this.closedDate = closedDate;
            return this;
        }

        public IssueBuilder withProjectName(String projectName){
            this.projectName = projectName;
            return this;
        }

        public IssueBuilder withStatusDesc(String statusDesc){
            this.statusDesc = statusDesc;
            return this;
        }

        public IssueBuilder withUpdated(Timestamp updated){
            this.updated = updated;
            return this;
        }

        public Issue build() {
            return new Issue(ID, name, description, creationDate, closedDate, projectName, statusDesc,updated);
        }


    }
}
