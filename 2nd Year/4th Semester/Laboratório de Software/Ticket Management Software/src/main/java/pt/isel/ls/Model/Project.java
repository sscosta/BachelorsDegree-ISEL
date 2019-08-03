package pt.isel.ls.Model;

import java.util.LinkedList;
import java.util.Objects;

public class Project {
    private LinkedList<ProjectLabel> labelList;
    public  String name;
    public String description;
    public java.sql.Date creationDate;
    public int ID;

    public Project(String name, String description, int ID,java.sql.Date creationDate) {
        this.name = name;
        this.description = description;
        this.ID = ID;
        this.creationDate = creationDate;
        this.labelList = null;
    }

    public Project() {
    }

    public void setLabels(LinkedList<ProjectLabel> labelsInProject) { labelList=labelsInProject;}
    public LinkedList<ProjectLabel> getLabels(){return labelList;}

    @Override
    public String toString() {
        return "Project:\n" +
                "\t" + name + '\n' +
                "\tDescription -> " + description + '\n' +
                "\tID -> " + ID + '\n' +
                "\tCreation Date -> " + creationDate.toString();
    }

    public static final class ProjectBuilder {
        private String name;
        private String description;
        private int ID;
        private java.sql.Date creationDate;

        public ProjectBuilder() {
        }

        public static ProjectBuilder aProject() {
            return new ProjectBuilder();
        }

        public ProjectBuilder withName(String name) {
            this.name = name;
            return this;
        }

        public ProjectBuilder withDescription(String description) {
            this.description = description;
            return this;
        }

        public ProjectBuilder withID(int ID) {
            this.ID = ID;
            return this;
        }
        public ProjectBuilder withCreationDate(java.sql.Date creationDate){
            this.creationDate = creationDate;
            return this;
        }

        public Project build() {
            return new Project(name, description, ID,creationDate);
        }
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Project project = (Project) o;
        return ID == project.ID &&
                Objects.equals(name, project.name) &&
                Objects.equals(description, project.description) &&
                Objects.equals(creationDate, project.creationDate);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, description, creationDate, ID);
    }
}
