package pt.isel.ls.Model;

public class IssueLabel {

    public String labelDesc;
    public int issueID;
    public String projectName;

    public IssueLabel(String labelDesc, int issueID, String projectName) {
        this.labelDesc = labelDesc;
        this.issueID = issueID;
        this.projectName = projectName;
    }

    public static final class IssueLabelBuilder {
        String labelDesc;
        int    issueID;
        String projectName;

        public IssueLabelBuilder() {
        }

        public static IssueLabelBuilder anIssueLabel() {
            return new IssueLabelBuilder();
        }

        public IssueLabelBuilder withLabelDesc(String labelDesc) {
            this.labelDesc = labelDesc;
            return this;
        }

        public IssueLabelBuilder withIssueID(int issueID) {
            this.issueID = issueID;
            return this;
        }

        public IssueLabelBuilder withProjectName(String projectName) {
            this.projectName = projectName;
            return this;
        }

        public IssueLabel build() {
            return new IssueLabel(labelDesc, issueID, projectName);
        }
    }
}
