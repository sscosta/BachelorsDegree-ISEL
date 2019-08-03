using ColumnSelector;
using Csvier.Managers;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace Csvier
{
    class MemberManager<T>
    {
        Type klass;

        List<Member> members = new List<Member>();
        List<int> csvCols = new List<int>();
        //Adds support to arrays member
        private List<Member> arrayMembers = new List<Member>();
        private List<Pair<int, int>> arrayCols = new List<Pair<int, int>>();

        public MemberManager()
        {
            this.klass = GetType().GetGenericArguments()[0];
        }

        public void AddField(string name, int col)
        {
            FieldInfo fi = klass.GetField(name);
            if (fi == null)
                throw new InvalidOperationException(String.Format("There is no field {0}", name));
            members.Add(new Field(fi));
            csvCols.Add(col);
        }

         public void AddFieldArray(string name, int colBeg, int colEnd)
        {
            FieldInfo fi = klass.GetField(name);
            if (fi == null)
                throw new InvalidOperationException(String.Format("There is no field {0}", name));
            arrayMembers.Add(new Field(fi));
            arrayCols.Add(new Pair<int, int>(colBeg, colEnd));
        }

        public void AddProperty(string name, int col)
        {
            PropertyInfo pi = klass.GetProperty(name);
            if (pi == null)
                throw new InvalidOperationException(String.Format("There is no property {0}", name));
            members.Add(new Property(pi));
            csvCols.Add(col);
        }

        public void AddPropertyArray(string name, int colBeg, int colEnd)
        {
            PropertyInfo pi = klass.GetProperty(name);
            if (pi == null)
                throw new InvalidOperationException(String.Format("There is no property {0}", name));
            arrayMembers.Add(new Property(pi));
            arrayCols.Add(new Pair<int, int>(colBeg, colEnd));
        }

        public void SetValues<T>(T target, CsvParser<T> csvParser)
        {
            AddMembersFromCustomAttributes();
            AddSimpleMembers(target, csvParser);
            AddArrayMembers(target, csvParser);
        }

        private void AddSimpleMembers<T>(object target, CsvParser<T> csvParser)
        {
            for (int i = 0; i < members.Count; i++)
            {
                Member m = members[i];
                m.SetValue(target, csvParser.GetParsedValue(members[i].GetMemberType(), csvParser.CurrentLine().ElementAt(csvCols[i])));
            }
        }

        private void AddArrayMembers<T>(T target, CsvParser<T> csvParser)
        {
            for (int i = 0; i < arrayMembers.Count; i++)
            {
                Member m = arrayMembers[i];
                m.SetValue(target, csvParser.GetParsedArrayValue(members[i].GetMemberType(), csvParser.CurrentLine(), arrayCols[i]));
            }
        }

        private void AddMembersFromCustomAttributes()
        {
            AddFieldsFromCustomAttributes();
            AddPropertiesFromCustomAttributes();
        }

        private void AddFieldsFromCustomAttributes()
        {
            foreach(FieldInfo fi in klass.GetFields())
            {
                foreach(ConfiguratorAttribute ca in (ConfiguratorAttribute[])fi.GetCustomAttributes(typeof(ConfiguratorAttribute), false))
                {
                    AddField(ca.GetName(), ca.GetCol());
                }
            }
        }

        private void AddPropertiesFromCustomAttributes()
        {
           foreach (PropertyInfo pi in klass.GetProperties())
            {
                foreach (ConfiguratorAttribute ca in (ConfiguratorAttribute[])pi.GetCustomAttributes(typeof(ConfiguratorAttribute), false))
                {
                    AddProperty(ca.GetName(), ca.GetCol());
                }
            }
        }
    }
}
