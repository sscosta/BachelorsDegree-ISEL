package pt.isel.ls.PresentationLayer.View;

import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.Model.Comment;
import pt.isel.ls.PresentationLayer.View.Html.*;
import pt.isel.ls.ResultType.*;
import pt.isel.ls.ResultType.ResultGet.*;
import pt.isel.ls.ResultType.ResultPost.*;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public class ViewFactory {

    public static final Map<Class, Class> viewHtmlMap;
    static {
        Map<Class, Class> resultViewTypeMap = new HashMap<>();
        resultViewTypeMap.put(ResultGetIssueById.class, SingleIssueView.class);
        resultViewTypeMap.put(ResultGetIssuesFromProjectWithPid.class, IssueListView.class);
        resultViewTypeMap.put(ResultGetIssues.class, IssueListView.class);
        resultViewTypeMap.put(ResultGetProjects.class, ProjectsListView.class);
        resultViewTypeMap.put(ResultGetProjectsOrderedByClosedIssueCount.class, ProjectsListView.class);
        resultViewTypeMap.put(ResultGetProjectsOrderedByCreationDate.class, ProjectsListView.class);
        resultViewTypeMap.put(ResultGetProjectsOrderedByIssueCount.class, ProjectsListView.class);
        resultViewTypeMap.put(ResultGetProjectsOrderedByOpenIssueCount.class, ProjectsListView.class);
        resultViewTypeMap.put(ResultGetProjectWithId.class, SingleProjectView.class);
        resultViewTypeMap.put(ResultListOptions.class, OptionView.class);
        resultViewTypeMap.put(ResultRoot.class,RootView.class);
        resultViewTypeMap.put(ResultSearchProjects.class, SearchView.class);
        resultViewTypeMap.put(ResultGetComments.class, CommentView.class);
        resultViewTypeMap.put(ResultGetIssuesWithLabel.class,LabelView.class);
        resultViewTypeMap.put(ResultGetLabelsInProject.class,ProjectLabelView.class);
        resultViewTypeMap.put(ResultGetLabelsInIssue.class, IssueLabelView.class);
        resultViewTypeMap.put(ResultGetLabelDetail.class, ProjectLabelDetailView.class);

        resultViewTypeMap.put(ResultPostLabel.class, ProjectLabelDetailView.class);
        resultViewTypeMap.put(ResultUpdateIssue.class, SingleIssueView.class);
        resultViewTypeMap.put(ResultPostComment.class, CommentView.class);
        resultViewTypeMap.put(ResultPostIssue.class, SingleIssueView.class);
        resultViewTypeMap.put(ResultPostProjects.class, SingleProjectView.class);
        resultViewTypeMap.put(ResultPostIssueLabel.class, IssueLabelView.class);




        viewHtmlMap = Collections.unmodifiableMap(resultViewTypeMap);
    }

    public static String getHtmlView(Result result, RequestContext requestContext) throws NoSuchMethodException, IllegalAccessException, InvocationTargetException, InstantiationException {
        Class view = viewHtmlMap.get(result.getClass());
        Constructor ctor = view.getConstructor(Result.class, RequestContext.class);

        return ((ViewHtml)ctor.newInstance(result, requestContext)).getHtml();
    }
}
