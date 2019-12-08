#include <QtTest/QtTest>

class TestQString : public QObject
{
    QObject

    private slots: 
        void toUpper();
};

void TestQString::toUpper()
{
    QString str = "Hello, test!";
    QCOMPARE(str.toUpper(), "HELLO, TEST!");
}

QTEST_MAIN(TestQString)
#include "ApplicationTests.moc"
