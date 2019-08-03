package pt.isel.ls.Model;


public class Comment {
    public int id;
    public int issueID;
    public String projectName;
    public java.sql.Timestamp date;
    public String text;

    public Comment(int id, int issueID, String projectName, java.sql.Timestamp date, String text) {
        this.id = id;
        this.issueID = issueID;
        this.projectName = projectName;
        this.date = date;
        this.text = text;
    }
    public Comment() {
    }

    @Override
    public String toString() {
        return "Comment:\n" +
                "\tComment id -> " + id + '\n' +
                "\tIssue ID -> " + issueID + '\n' +
                "\tProject Name -> " + projectName + '\n' +
                "\tCreation Date -> " + date + '\n' +
                "\tText -> " + text;
    }

    public static final class CommentBuilder {
        private int    id;
        private int    issueID;
        private String projectName;
        private java.sql.Timestamp   date;
        private String text;

        public CommentBuilder(){}

        public static CommentBuilder aProject() {
            return new CommentBuilder();
        }

        public CommentBuilder withId(int id) {
            this.id = id;
            return this;
        }

        public CommentBuilder withIssueID(int issueID) {
            this.issueID = issueID;
            return this;
        }

        public CommentBuilder withProjectName(String projectName) {
            this.projectName = projectName;
            return this;
        }

        public CommentBuilder withDate(java.sql.Timestamp date) {
            this.date = date;
            return this;
        }

        public CommentBuilder withText(String text) {
            this.text = text;
            return this;
        }

        public Comment build() {
            return new Comment(id,issueID,projectName,date,text);
        }
    }
}
