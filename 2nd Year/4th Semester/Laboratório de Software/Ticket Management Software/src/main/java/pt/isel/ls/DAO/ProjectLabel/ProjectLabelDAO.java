package pt.isel.ls.DAO.ProjectLabel;

import pt.isel.ls.Exceptions.InvalidParamException;
import pt.isel.ls.Model.ProjectLabel;

import java.util.LinkedList;

public interface ProjectLabelDAO {

    LinkedList<ProjectLabel> getLabelsForProject(String projectName) throws InvalidParamException;
    ProjectLabel getLabelDetail(String label, String projectName)throws InvalidParamException;
}
