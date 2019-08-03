package pt.isel.ls.Model;

public class ProjectLabel {

    public String projectName;
    public String labelDesc;
    public String color;

    public ProjectLabel(String projectName, String labelDesc, String color) {
        this.projectName = projectName;
        this.labelDesc = labelDesc;
        this.color = color;
    }

    @Override
    public String toString() {
        return "Project Label:\n" +
                "\tProject Name ->" + projectName + '\n' +
                "\tLabel Description -> " + labelDesc + '\n' +
                "\tColor ->" + color;
    }

    public static final class ProjectLabelBuilder {
        String projectName;
        String labelDesc;
        String color;

        public ProjectLabelBuilder() {
        }

        public static ProjectLabelBuilder aProjectLabel() {
            return new ProjectLabelBuilder();
        }

        public ProjectLabelBuilder withProjectName(String projectName) {
            this.projectName = projectName;
            return this;
        }

        public ProjectLabelBuilder withLabelDesc(String labelDesc) {
            this.labelDesc = labelDesc;
            return this;
        }

        public ProjectLabelBuilder withColor(String color) {
            this.color = color;
            return this;
        }

        public ProjectLabel build() {
            return new ProjectLabel(projectName, labelDesc, color);
        }
    }
}
