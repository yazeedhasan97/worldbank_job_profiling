# Generated by Django 3.1.2 on 2021-07-17 16:09

from django.db import migrations


class Migration(migrations.Migration):

    dependencies = [
        ('api', '0003_evaluationoutput_run_time'),
    ]

    operations = [
        migrations.DeleteModel(
            name='EvaluationOutput',
        ),
    ]
